class ULTerm

    # By default, we assume terms are irreducible,
    # not abstractions, and not values.
    # Subclasses which should have these properties
    # must override these methods.
    # (In our basic calculus with call-by-value semantics,
    # only applications are reducible and only abstractions
    # are values. This can be changed for different calculi/semantics.)
    def reduce; nil end
    def absBody; nil end
    def isValue?; false end
  
    # Shifting is just walking, where in the base case,
    # we either increment the variable by shiftAmount or
    # leave it alone.
    def shift(shiftAmount)
      # walk is an iterator.
      # The block tells us what to do with variables.
      walk(0) { |x,currentBinders|
        if x >= currentBinders
          ULVar.new(x+shiftAmount)
        else
          ULVar.new(x)
        end }
    end
  
    # Substitution is just walking, where we either
    # replace the variable, or leave it alone.
    def substitute(x,r)
      walk(0) { |y,currentBinders|
        if y == x + currentBinders
          r
        else
          ULVar.new(y)
        end }
    end
  
    def eval
      r = nil
      r_next = self
      # Keep reducing until it fails (reduce returns nil.)
      # This is the recommended "do...while" form in Ruby.
      loop do
        r = r_next
        r_next = r.reduce
        break unless r_next
      end
  
      return r
    end
  end


  class ULVar < ULTerm
    attr_reader :index
  
    # We require our variables are only indexed by integers.
    def initialize(i)
      unless i.is_a?(Integer)
        throw "Constructing a lambda term out of non-lambda terms"
      end
      @index = i
    end

    # for ULVar, return the corresponding value of index by using getAlphabets function
    def prettify
      return getAlphabets(self.index)
    end
    
    def walk(currentBinders,&block)
      # This is a variable. Run the code in &block.
      # (yield does this; it "yields" control to the block.)
      yield(@index, currentBinders)
    end
  
    def to_s
      @index.to_s
    end
  end
  
  class ULAbs < ULTerm
    attr_reader :t
  
    def initialize(t)
      unless t.is_a?(ULTerm)
        throw "Constructing a lambda term out of a non-lambda term"
      end
      @t = t
    end

    # construct first part(lambda) by taking the number of Abs as input, and the second part prettify as usual
    def prettify
      return constructLambda(countAbs(self.t)+1).to_s + reduceAbs(self.t).prettify
    end
    
    def walk(currentBinders,&block)
      # Increment the local variable counter within the variable binder.
      t = @t.walk(currentBinders+1,&block)
      ULAbs.new(t)
    end
  
    # Abstractions are an abstraction (of course),
    # with body @t,
    # and are also considered values.
    def absBody; @t end
    def isValue?; true end
    
    def to_s
      "lambda . " + @t.to_s
    end
  end
  
  class ULApp < ULTerm
    attr_reader :t1
    attr_reader :t2
  
    def initialize(t1,t2)
      unless t1.is_a?(ULTerm) && t2.is_a?(ULTerm)
        throw "Constructing a lambda term out of non-lambda terms"
      end
      @t1 = t1; @t2 = t2
    end

    # return combination of prettify version of part1 and part2 
    def prettify
      return "("+ self.t1.prettify + ") (" + self.t2.prettify + ")"
    end
    
    def walk(currentBinders,&block)
      t1 = @t1.walk(currentBinders,&block)
      t2 = @t2.walk(currentBinders,&block)
      ULApp.new(t1,t2)
    end
  
    # Applications can be reduced.
    def reduce
      if @t1.absBody && @t2.isValue?
        body = @t1.absBody
        (body.substitute(0,@t2.shift(1))).shift(-1)
      elsif @t1.isValue?
        r = @t2.reduce
        if r
          ULApp.new(@t1,r)
        else
          nil
        end
      else
        r = @t1.reduce
        if r
          ULApp.new(r,@t2)
        else
          nil
        end
      end
    end
  
    def to_s
      "(" + @t1.to_s + ") (" + @t2.to_s + ")" 
    end
  end

  
# convert index to alphabets, 0 is a, 1 is b...27 is aa etc.
def getAlphabets(i)
    if (i<=26)
      return (i+97).chr #a is 97 according to ASCII table
    else 
      return getAlphabets(i/26-1)+getAlphabets(i%26-1) #minus one to ensure it start from zero
    end
end 

# count how many Abs is included in input in order to know how many lambda needed
def countAbs(input)
  case input
  when ULAbs
    return countAbs(getFirstAbs(input))+1
  when ULApp
    return 0
  end
end

# construct lambda by the result of countAbs
def constructLambda(i)
  if (i>0)
    return "lambda " + getAlphabets(i-1) + " . " +constructLambda(i-1)
  else 
    return ""
  end
end

# get the value of a Abs, ie. return appvars for lappvars = ULAbs.new(appvars)
def getFirstAbs(input)
  return input.t
end

# remove the Abs, ie. return appvars for lllappvars = ULAbs.new(ULAbs.new(lappvars))
def reduceAbs(input)
  case input
  when ULAbs
    return reduceAbs(getFirstAbs(input))
  when ULApp
    return input
  end
end
