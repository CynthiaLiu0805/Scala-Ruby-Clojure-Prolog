class VarExpr

end

class Const < VarExpr
    attr_accessor :value
    def initialize(value=nil)
        @value = value
    end

    def interpret
        return self.value
    end
end

def construct_const(i) 
    con=Const.new(i)
    return con
end

class Neg < VarExpr
    attr_accessor :value
    def initialize(value=nil)
        @value = value
    end

    def interpret
        return -self.value.interpret
    end
end

def construct_neg(i) 
    n=Neg.new(i)
    return n
end

class Abs < VarExpr
    attr_accessor :value
    def initialize(value=nil)
        @value = value
    end

    def interpret
        return self.value.interpret.abs
    end
end

def construct_abs(i) 
    a=Abs.new(i)
    return a
end

class Plus < VarExpr
    attr_accessor :value1
    attr_accessor :value2
    def initialize(value1, value2)
        @value1=value1
        @value2=value2
    end

    def interpret
        return self.value1.interpret+self.value2.interpret
    end

end
def construct_plus(x,y) 
    p=Plus.new(x,y)
    return p
end

class Times < VarExpr
    attr_accessor :value1
    attr_accessor :value2
    def initialize(value1, value2)
        @value1=value1
        @value2=value2
    end

    def interpret
        return self.value1.interpret*self.value2.interpret
    end

end
def construct_times(x,y) 
    t=Times.new(x,y)
    return t
end

class Minus < VarExpr
    attr_accessor :value1
    attr_accessor :value2
    def initialize(value1, value2)
        @value1=value1
        @value2=value2
    end

    def interpret
        return self.value1.interpret-self.value2.interpret
    end

end
def construct_minus(x,y) 
    m=Minus.new(x,y)
    return m
end

class Exp < VarExpr
    attr_accessor :value1
    attr_accessor :value2
    def initialize(value1, value2)
        @value1=value1
        @value2=value2
    end

    def interpret
        return self.value1.interpret**self.value2.interpret
    end

end
def construct_exp(x,y) 
    e=Exp.new(x,y)
    return e
end

class Var < VarExpr
    attr_accessor :value
    def initialize(value=nil)
        @value = value
    end

    def interpret
        return self.value
    end
end

def construct_var(x) 
    v=Var.new(x)
    return v
end

class Subst < VarExpr
    attr_accessor :value1
    attr_accessor :value2
    attr_accessor :value3

    def initialize(value1, value2,value3)
        @value1=value1
        @value2=value2
        @value3=value3
    end

    def interpret
        return substitution(value1,value2,value3).interpret
    end
end

def construct_subst(x,y,z) 
    s=Subst.new(x,y,z)
    return s
end

# helper methods to get first/second/third value
def get_first(e)
    return e.value1
end
def get_second(e)
    return e.value2
end
def get_third(e)
    return e.value3
end

# a helper method to do substitution
def substitution(value1,value2,value3)
    case value1
    when Var
        return value3
    when Abs
        return construct_abs(value3)
    when Neg
        return construct_neg(value3)
    when Plus
        case get_first(value1)
        when Var
            return construct_plus(value3,get_second(value1))
        else
            return construct_plus(get_first(value1),value3)
        end
    when Minus
        case get_first(value1)
        when Var
            return construct_minus(value3,get_second(value1))
        else
            return construct_minus(get_first(value1),value3)
        end
    when Times
        case get_first(value1)
        when Var
            return construct_times(value3,get_second(value1))
        else
            return construct_times(get_first(value1),value3)
        end
    when Exp
        case get_first(value1)
        when Var
            return construct_exp(value3,get_second(value1))
        else
            return construct_exp(get_first(value1),value3)
        end
    when Subst
        substitution(substitution(get_first(value1),get_second(value1),get_third(value1)),value2,value3)
    end

end

e1=construct_const(-3)
e2 = construct_const(4)
e3=construct_var(:x)
e4=construct_var(:y)
e5 = construct_exp(e3,e4)
e6=construct_abs(e3)

puts("test case for Var: e3.interpret==:x")
puts(e3.interpret==:x)

puts("test case for Abs: construct_subst(e6,:x,e1).interpret==3")
puts(construct_subst(e6,:x,e1).interpret==3)

puts("test case for Subst: construct_subst(construct_subst(e5,:x,e1),:y,e2).interpret==81")
puts(construct_subst(construct_subst(e5,:x,e1),:y,e2).interpret==81)