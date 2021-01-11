require_relative "a2_ulterm"
class STType end
  class STNat < STType
    # Comparison and printing methods
    def ==(type); type.is_a?(STNat) end
    def to_s; "nat" end
  end
  
  class STBool < STType
    # Comparison and printing methods
    def ==(type); type.is_a?(STBool) end
    def to_s; "bool" end
  end
  
  # Functions have a domain type and a codomain type.
  class STFun < STType
    attr_reader :dom
    attr_reader :codom
    
    def initialize(dom, codom)
      unless dom.is_a?(STType) && dom.is_a?(STType)
        throw "Constructing a type out of non-types"
      end
      @dom = dom; @codom = codom
    end
  
    # Comparison and printing methods
    def ==(type); type.is_a?(STFun) && type.dom == @dom && type.codom == @codom end 
    def to_s; "(" + dom.to_s + ") -> (" + codom.to_s + ")" end
  end
  


class STTerm 
  def typecheck
    if typeOf(Array.new)==nil
      return false
    else
      return true
    end
  end
end

  class STVar < STTerm
    attr_reader :index
  
    # We require our variables are only indexed by integers.
    def initialize(index)
      unless index.is_a?(Integer) 
        throw "Constructing a STVar term out of non-integer terms"
      end
      @index = index
    end

    def ==(type); type.is_a?(STVat) && type.index==@index end
    def typeOf(arr); nil end
    def eraseTypes; ULVar.new(index) end
  end


  class STApp < STTerm
    attr_reader :t1
    attr_reader :t2
    def initialize(t1,t2)
      unless t1.is_a?(STTerm) && t2.is_a?(STTerm)
        throw "Constructing a lambda term out of non-lambda terms"
      end
      @t1 = t1; @t2 = t2
    end
    def ==(type); type.is_a?(STApp) && type.t1==@t1 && type.t2==@t2 end

    def typeOf(arr)
      case t1.typeOf(Array.new)
      when STFun
        if t1.typeOf(Array.new).dom == t2.typeOf(Array.new)
          return t1.typeOf(Array.new).codom
        else
          return nil
        end
      else 
        return nil
      end
    end

    def eraseTypes; ULApp.new(t1.eraseTypes,t2.eraseTypes) end
  end  

  class STAbs < STTerm
    attr_reader :t1
    attr_reader :t2
    def initialize(t1,t2)
      unless t1.is_a?(STType) && t2.is_a?(STTerm)
        throw "Constructing a lambda term out of non-lambda terms"
      end
      @t1 = t1; @t2 = t2
    end

    def ==(type); type.is_a?(STAbs) && type.t1==@t1 && type.t2==@t2 end

    def typeOf(arr)
      case t2
      when STVar
        if (arr<<t1)[t2.index]==t1
          return STFun.new(t1,STNat.new)
        else 
          return nil
        end
      when STApp
        if (arr<<t1)[0].dom == t1
          return STNat.new
        end
      else 
        return STFun.new(t1,t2.typeOf(arr<<t1))
      end
    end

    def eraseTypes; ULAbs.new(t2.eraseTypes) end
  end  

  class STZero < STTerm
    def ==(type); type.is_a?(STZero) end
    def to_s; "zero" end
    def typeOf(arr); STNat.new end
    def eraseTypes; ULAbs.new(ULAbs.new(ULVar.new(0))) end
  end

  class STSuc < STTerm
    attr_reader :t
    def initialize(t)
      unless t.is_a?(STTerm)
        throw "Constructing a lambda term out of non-lambda terms"
      end
      @t = t
    end

    def ==(type); type.is_a?(STSuc) && type.t==@t end

    def typeOf(arr)
      if t.typeOf(Array.new)==STNat.new
        return STNat.new
      else 
        return nil
      end  
    end

    def eraseTypes; ULApp.new(ULAbs.new(ULAbs.new(ULAbs.new(ULApp.new(ULVar.new(1),ULApp.new(ULApp.new(ULVar.new(2),ULVar.new(1)),ULVar.new(0)))))),t.eraseTypes) end

  end  

  class STIsZero < STTerm
    attr_reader :t
    def initialize(t)
      unless t.is_a?(STTerm)
        throw "Constructing a lambda term out of non-lambda terms"
      end
      @t = t
    end

    def ==(type); type.is_a?(STIsZero) && type.t==@t end
    def typeOf(arr)
      if t.typeOf(Array.new)==STNat.new
        return STBool.new
      else 
        return nil
      end  
    end

    def eraseTypes; ULAbs.new(ULApp.new(ULApp.new(t.eraseTypes,ULAbs.new((STFalse.new).eraseTypes)), (STTrue.new).eraseTypes)) end;
  end  

  class STTrue < STTerm
    def ==(type); type.is_a?(STTrue) end
    def to_s; "true" end
    def typeOf(arr); STBool.new end
    def eraseTypes; ULAbs.new(ULAbs.new(ULVar.new(1))) end

  end

  class STFalse < STTerm
    def ==(type); type.is_a?(STFalse) end
    def to_s; "false" end
    def typeOf(arr); STBool.new end
    def eraseTypes; ULAbs.new(ULAbs.new(ULVar.new(0))) end
  end

  class STTest < STTerm
    attr_reader :t1
    attr_reader :t2
    attr_reader :t3
    def initialize(t1,t2,t3)
      unless t1.is_a?(STTerm) && t2.is_a?(STTerm) && t2.is_a?(STTerm)
        throw "Constructing a lambda term out of non-lambda terms"
      end
      @t1 = t1; @t2 = t2; @t3=t3
    end

    def ==(type); type.is_a?(STTest) && type.t1==@t1 && type.t2==@t2 && type.t3==@t3 end

    def typeOf(arr)
      if t1.typeOf(arr) != STBool.new
        return nil
      else 
        return t2.typeOf(Array.new) 
      end
    end

    def eraseTypes; ULAbs.new(ULAbs.new(ULAbs.new(ULApp.new(ULApp.new(t1.eraseTypes,t2.eraseTypes),t3.eraseTypes)))) end
  end  

