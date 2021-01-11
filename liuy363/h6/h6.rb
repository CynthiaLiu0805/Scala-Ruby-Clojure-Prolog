class Expr

    # define later
    def interpret
    end

end

class Const < Expr
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

class Neg < Expr
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

class Abs < Expr
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

class Plus < Expr
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

class Times < Expr
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

class Minus < Expr
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

class Exp < Expr
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
