def fizzbuzzLooper(l)
    result=Array.new
    for element in l do
        if element%3==0 && element%5==0
            result.push("fizzbuzz")
        
        elsif element%3==0
            result.push("fizz")
        
        elsif element%5==0
            result.push("buzz")
        else 
            result.push(element.to_s)
        end
    end
    return result
end


# print(fizzbuzzLooper([2,3,4,5]))


def fizzbuzzIterator(l)
    result=Array.new
    l.each do |element|
        if element%3==0 && element%5==0
            result.push("fizzbuzz")
        
        elsif element%3==0
            result.push("fizz")
        
        elsif element%5==0
            result.push("buzz")
        else 
            result.push(element.to_s)
        end
    end
    return result
end

def second(arr)
    arr.length <= 1 ? nil : arr[1]
end

def zuzzer(l,rule)
    result=Array.new
    l.each do |e|
        tmp=e.to_s
        rule.each do |r|
            # print(e)
            if r.first.(e) 
                if (tmp.to_i.to_s==tmp)
                    tmp=second(r).(e)
                else
                    tmp+=second(r).(e)
                end
            else
                tmp=tmp
            end
        end  #end of rule
        result.push(tmp)
    end
    return result
end

