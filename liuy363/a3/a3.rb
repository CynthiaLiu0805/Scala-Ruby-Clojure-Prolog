module GCL
class GCExpr end

    class GCConst < GCExpr
        attr_reader :i
        def initialize(i)
          unless i.is_a?(Integer) 
            throw "The constructor needs to be an integer"
          end
          @i = i
        end

        def wellScoped(variable); true end

    end

    class GCVar < GCExpr
        attr_reader :s
      
        def initialize(s)
          unless s.is_a?(Symbol) 
            throw "The constructor needs to be a symbol"
          end
          @s = s
        end

        def wellScoped(variable); variable.include?(s) end

    end

    class GCOp < GCExpr
        attr_reader :a1
        attr_reader :a2
        attr_reader :s

        def initialize(a1,a2,s)
          unless a1.is_a?(GCExpr) && a2.is_a?(GCExpr) && s==:plus||:times||:minus||:div
            throw "The constructors' type do not match"
          end
          @a1 = a1
          @a2 = a2
          @s = s
        end

        def wellScoped(variable)
          return a1.wellScoped(variable) && a2.wellScoped(variable)
        end

    end

class GCTest end
    class GCComp < GCTest
      attr_reader :a1
      attr_reader :a2
      attr_reader :s

      def initialize(a1,a2,s)
        unless a1.is_a?(GCExpr) && a2.is_a?(GCExpr) && s==:eq||:less||:greater
          throw "The constructors' type do not match"
        end
        @a1 = a1
        @a2 = a2
        @s = s
      end

      def wellScoped(variable)
        return a1.wellScoped(variable) && a2.wellScoped(variable)
      end

    end

    class GCAnd < GCTest
      attr_reader :a1
      attr_reader :a2

      def initialize(a1,a2)
        unless a1.is_a?(GCTest) && a2.is_a?(GCTest) 
          throw "The constructors' type do not match"
        end
        @a1 = a1
        @a2 = a2
      end

      def wellScoped(variable)
        return a1.wellScoped(variable) && a2.wellScoped(variable)
      end

    end

    class GCOr < GCTest
      attr_reader :a1
      attr_reader :a2

      def initialize(a1,a2)
        unless a1.is_a?(GCTest) && a2.is_a?(GCTest) 
          throw "The constructors' type do not match"
        end
        @a1 = a1
        @a2 = a2
      end

      def wellScoped(variable)
        return a1.wellScoped(variable) && a2.wellScoped(variable)
      end

    end

    class GCTrue < GCTest 
      def wellScoped(variable); true end
    end
    class GCFalse < GCTest 
      def wellScoped(variable); true end
    end

class GCStmt end
  class GCSkip < GCStmt 
    def wellScoped(variable); true end
  end

  class GCAssign < GCStmt
    attr_reader :s
    attr_reader :a

    def initialize(s,a)
      unless s.is_a?(Symbol) && a.is_a?(GCExpr)
        throw "The constructors' type do not match"
      end
      @s = s
      @a = a
    end

    def wellScoped(variable)
      return variable.include?(s) && a.wellScoped(variable) # check if s is in the variable and recurison the argument with the variables
    end

  end

  class GCCompose < GCStmt
    attr_reader :a1
    attr_reader :a2

    def initialize(a1,a2)
      unless a1.is_a?(GCStmt) && a2.is_a?(GCStmt)
        throw "The constructors' type do not match"
      end
      @a1=a1
      @a2=a2
    end

    def wellScoped(variable)
      return a1.wellScoped(variable) && a2.wellScoped(variable)
    end

  end

  class GCIf < GCStmt
    attr_reader :l
    def initialize(l)
      unless l.is_a?(Array) && l.all? { |x| (x[0].is_a? GCTest) && (x[1].is_a? GCStmt) }         #[[GCTest,GCStmt]]
        throw "The constructors' type do not match"
      end
      @l = l
    end

    # loop through each pair, if one of them is not wellScoped, it is not overall wellscoped.
    def wellScoped(variable)
      l.each { |pair|
        gcTest=pair[0]
        gcStmt=pait[1]
        if !(gcTest.wellScoped(variable) && gcStmt.wellScoped(variable))
          return false
        end
      }
      return true
    end
   
  end

  class GCDo < GCStmt
    attr_reader :l
    def initialize(l)
      unless l.is_a?(Array) && l.all? { |x| (x[0].is_a? GCTest) && (x[1].is_a? GCStmt) }         #[[GCTest,GCStmt]]
        throw "The constructors' type do not match"
      end
      @l = l
    end

    def wellScoped(variable)
      l.each { |pair|
        gcTest=pair[0]
        gcStmt=pait[1]
        if !(gcTest.wellScoped(variable) && gcStmt.wellScoped(variable))
          return false
        end
      }
      return true
    end

  end

# part 2: a stack machine
  def emptyState
    lambda { |_| 0}
  end

  def updateState(sigma, x, n)
    lambda { |y| if y==x then n else sigma[x] end}
  end

  def stackEval(command, result, memory) 
    until command.empty?
        x = command.pop
        case x
          when GCConst then result.push(x.i)
          when GCVar   then result.push(memory[x.s])
          when GCOp 
            command.push([:operation, x.s])
            command.push(x.a1)
            command.push(x.a2)
          when GCComp
            command.push([:composition, x.s])
            command.push(x.a1)
            command.push(x.a2)
          when GCAnd
            command.push([:and, :&])
            command.push(x.a1)
            command.push(x.a2)
          when GCOr
            command.push([:or, :|])
            command.push(x.a1)
            command.push(x.a2)
          when GCTrue
            result.push(true)
          when GCFalse
            result.push(false)
          when GCSkip
            return
          when GCAssign
            command.push([:assign, x.s])
            command.push(x.a)
          when GCCompose
            command.push(x.a2)
            command.push(x.a1)
          when GCIf
            command.push [:if, x.l.size]
            x.l.each{ |pair|
              command.push([:sublist, pair[1]])
              command.push(pair[0])
            }

          when GCDo
            command.push([:do, x.l.size, x])
            x.l.each{|pair|
              command.push([:sublist, pair[1]])
              command.push(pair[0])
            }
          when Array
            case x[0]
            when :operation,:composition,:and,:or
              b, a = result.pop(2)
              case x[1]
                when :plus  then result.push(a+b)
                when :times then result.push(a*b)
                when :minus then result.push(a-b)
                when :div   then result.push(a/b)
          
                when :eq    then result.push(a == b)
                when :greater then result.push(a > b)
                when :less  then result.push(a < b)

                when :&     then result.push(a && b)
                when :|     then result.push(a || b)
              end
            when :assign
              memory = updateState(memory, x[1], result.pop)
            when :sublist
              result.push([result.pop, x[1]])
            when :if
              size = x[1]
              ifList = result.pop(size).reverse  
              testList=Array.new  #GCTest
              # merge ifList elements together
              ifList.each do |n|
                testList<<n
              end
              if !testList.empty?
                command.push testList.sample[1] # sample gives a random elements
              end
            when :do
              size = x[1]
              doList = result.pop(size).reverse
              # select a new array containing all elements in array that first is a true value.
              testList = doList.select{|(first, second)| first}
              if !testList.empty?
                command.push x[2] # the do loop
                command.push testList.sample[1]
              end
            end
          end
      end
    return memory
  end
end

module GCLe 
  include GCL

class GCProgram
  attr_reader :a1
  attr_reader :a2

  def initialize(a1,a2)
    unless a1.all? { |x| (x.is_a? Symbol) } && a2.is_a?(GCL::GCStmt)
      throw "The constructors' type do not match"
    end
    @a1=a1
    @a2=a2
  end

  def wellScoped
    return a2.wellScoped(a1)
  end
  
end

class GCLocal < GCL::GCStmt
  attr_reader :a1
  attr_reader :a2

  def initialize(a1,a2)
    unless a1.is_a?(Symbol) && a2.is_a?(GCL::GCStmt)
      throw "The constructors' type do not match"
    end
    @a1=a1
    @a2=a2
  end

  def wellScoped(variable)
    return a2.wellScoped(variable.push(a1)) #push the current variable out and check next one
  end

end

# part 4: for GCLe
def wellScoped(gcProgram)
  return gcProgram.wellScoped()
end

def eval(program)
  newState = stackEval([program.a2],[],emptyState) 
  result = Hash.new
  program.a1.each { |var|
      result[var] = newState.call(var)
  }
  return result
end

end
