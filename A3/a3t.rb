def defclass(base, **fields)
  Class.new(base) {
    const_set :FIELDS, fields
    attr_accessor *self::FIELDS.map(&:first)
    def initialize(*args)
      self.class::FIELDS.zip(args).each{|(a, b)|
        if a[1] === b
          instance_variable_set "@#{a[0]}", b
        else
        raise TypeError.new("Should be #{a[1]} got #{b.class} #{b.inspect}")
        end
      }
    end

    def to_s
      "#{self.class.to_s.downcase.split("::")[-1]}(#{self.class::FIELDS.map{|x| self.send(x[0])}.join(",")})"
    end
    alias inspect to_s
  }
end

class OneOf
  def initialize(a)
     @arr = a
  end
  def ===(obj)
     @arr.include?(obj)
  end
  def to_s
    "one of #{@arr.inspect}"
  end
end

class ListOf
  def initialize(a)
    @type = a
  end
  def ===(obj)
    obj.all?{|x| @type === x }
  end
  def to_s
    "List[#{@type}]"
  end
end

class TupleOf
  def initialize(a)
    @types = a
  end
  def ===(obj)
    Array === obj && @types.size === obj.size && @types.zip(obj).all?{|(lhs, rhs)|
        lhs === rhs
    }
  end
  def to_s
    "Tuple[#{@types.join(",")}]"
  end
end

def oneof(arr)     OneOf.new(arr) end
def listof(type)   ListOf.new(type) end
def tupleof(type)  TupleOf.new(type) end

module GCL
  class GCExpr; end
  GCConst = defclass GCExpr, :num => Integer
  GCVar   = defclass GCExpr, :name => Symbol
  GCOp    = defclass GCExpr, :a => GCExpr, :b =>GCExpr, :op => oneof([:plus, :times, :minus, :div])
  class GCTest;  end

  GCComp = defclass GCTest, :a => GCExpr, :b => GCExpr, :op => oneof([:eq, :less, :greater])
  GCAnd = defclass GCTest, :a =>GCTest, :b => GCTest
  GCOr = defclass GCTest, :a =>GCTest, :b => GCTest


  GCTrue = defclass GCTest
  GCFalse = defclass GCTest

  class GCStmt; end
  GCSkip    = defclass GCStmt
  GCAssign  = defclass GCStmt, :name => Symbol, :value => GCExpr
  GCCompose = defclass GCStmt, :first => GCStmt, :second => GCStmt
  GCIf = defclass GCStmt, :branch => listof(tupleof([GCTest, GCStmt]))
  GCDo = defclass GCStmt, :branch => listof(tupleof([GCTest, GCStmt]))

  ######## operation??
  class GCAnd
    def op
      :&
    end
  end

  class GCOr
    def op
      :|
    end
  end

  module_function
  def emptyState
      lambda{|_| 0}    
  end

  def updateState(sigma, x, n)
      lambda{|y|
        if y == x then n else sigma[x] end
      }
  end

  def stackEval(cmd, result, mem) 
      until cmd.empty?
          x = cmd.pop
          case x
            when GCConst then result.push(x.num)
            when GCVar   then result.push(mem[x.name])
            when GCOp    then 
              cmd.push([:binary, x.op])
              cmd.push(x.a)
              cmd.push(x.b)
            when GCComp then
              cmd.push([:binary, x.op])
              cmd.push(x.a)
              cmd.push(x.b)
            when GCAnd then
              cmd.push([:binary, :&])
              cmd.push(x.a)
              cmd.push(x.b)
            when GCOr then
              cmd.push([:binary, :|])
              cmd.push(x.a)
              cmd.push(x.b)
            when GCTrue then
              result.push(true)
            when GCFalse then
              result.push(false)
            when GCSkip
            when GCAssign
              cmd.push([:assign, x.name])
              cmd.push(x.value)
            when GCCompose
              cmd.push(x.second)
              cmd.push(x.first)
            when GCIf
              cmd.push [:runif, x.branch.size]
              x.branch.each{|br|
                cmd.push [:sbranch, br[1]]
                cmd.push br[0]
              }

            when GCDo
              cmd.push [:rundo, x.branch.size, x]
              x.branch.each{|br|
                cmd.push [:cbranch, br[1]]
                cmd.push br[0]
              }

            when Array
              case x[0]
              when :binary
                b, a = result.pop(2)
                case x[1]
                when :plus  then result.push(a+b)
                when :times then result.push(a*b)
                when :minus then result.push(a-b)
                when :div   then result.push(a/b)
                when :&     then result.push(a && b)
                when :|     then result.push(a || b)
                when :eq    then result.push(a == b)
                when :greater then result.push(a > b)
                when :less  then result.push(a < b)
                end
              when :assign
                mem = updateState(mem, x[1], result.pop)
              when :sbranch, :cbranch
                result.push([result.pop, x[1]])
              when :runif
                sz = x[1]
                sp = result.pop(sz).reverse
                q = sp.select{|(cond, body)| cond }
                if !q.empty?
                  cmd.push q.sample[1]
                end
              when :rundo
                sz = x[1]
                doloop = x[2]
                sp = result.pop(sz).reverse
                q = sp.select{|(cond, body)| cond }
                if !q.empty?
                  cmd.push doloop
                  cmd.push q.sample[1]
                end
              end
            end
      end
      mem
  end
end

module GCLe
  include GCL

  GCProgram = defclass Object, :globals => listof(Symbol), :body => GCStmt
  GCLocal   = defclass GCStmt, :name => Symbol, :body => GCStmt

  module_function 
  def wellScopedWithEnv(term, env)
      case term
            when GCConst, GCTrue, GCFalse, GCSkip
               true
            when GCVar
               env.include?(term.name)
            when GCOp, GCComp, GCAnd, GCOr
              wellScopedWithEnv(term.a, env) && wellScopedWithEnv(term.b, env)
            when GCAssign
              env.include?(term.name) && wellScopedWithEnv(term.value, env)
            when GCCompose
              wellScopedWithEnv(term.first, env) && wellScopedWithEnv(term.second, env)
            when GCIf, GCDo
              term.branch.all?{|br| wellScopedWithEnv(br[0], env) && wellScopedWithEnv(br[1], env) }
            when GCLocal
              wellScopedWithEnv(term.body, env + [term.name]) 
            end
  end

  def wellScoped(prog)
      wellScopedWithEnv(prog.body, prog.globals) 
  end

  UninitializedException = Class.new(Exception)
  def evalWithEnv(term, global, local)
      case term
            when GCConst
               term.num
            when GCTrue
               true
            when GCFalse
               false
            when GCSkip
               nil
            when GCVar
              u = local.include?(term.name) ? local[term.name] :
                  global.include?(term.name) ? global[term.name] :                 
                  :exc
              raise UninitializedException.new("#{term.name} is not initialized") if u == :exc
              u
            when GCOp, GCComp, GCAnd, GCOr
               a, b = evalWithEnv(term.a, global, local), evalWithEnv(term.b, global, local)
               case term.op
               when :plus  then a + b
               when :times then a * b
               when :minus then a - b
               when :div   then a / b
               when :&     then a && b
               when :|     then a || b
               when :eq    then a == b
               when :greater then a > b
               when :less  then a <b
               end
            when GCAssign
              rhs = evalWithEnv(term.value, global, local)
              if local.include?(term.name)
                local
              elsif global.include?(term.name)
                global
              end[term.name] = rhs
            when GCCompose
              evalWithEnv(term.first, global, local)
              evalWithEnv(term.second, global, local)
            when GCIf
              q = term.branch.select{|(cond, body)|
                  evalWithEnv(cond, global, local)
              }
              evalWithEnv(q.sample[1], global, local) unless q.empty?
            when GCDo
              loop do
                q = term.branch.select{|(cond, body)|
                   evalWithEnv(cond, global, local)
                }
                break if q.empty?
                evalWithEnv(q.sample[1], global, local)
              end
            when GCLocal
              evalWithEnv(term.body, global, {**local, term.name => :exc})
            end
  end

  def eval(prog)
      env = prog.globals.map{|x| [x, :exc]}.to_h
      evalWithEnv(prog.body, env, {})
      env
  end
end

=begin
GCLe.constants.each{|x|
    kls = GCLe.const_get(x)
    if kls.constants.include?(:FIELDS)
       fields = kls::FIELDS
       name = kls.to_s.split("::")[-1]
       puts "(defrecord #{name} [#{fields.map(&:first).join(" ")}])"
    end
}
=end
