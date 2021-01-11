require_relative 'collection'
Pair = Struct.new(:fst,:snd)
def summingPairs(xs, sum)
  the_pairs1 = []
  the_pairs2 = []
  the_pairs = []
  len = xs.length

  reader, writer = IO.pipe 
  
    fork do
     for i in 0..(len-1)
      for j in (i+1)..(len/2)
        if xs[i] + xs[j] <= sum
            writer.puts(Pair.new(xs[i],xs[j]))
        end
      end
    end
  end
  
    fork do
      for i in 0..(len-1)
         for j in (len/2)..(len-1)
             if xs[i] + xs[j] <= sum
                  writer.puts(Pair.new(xs[i],xs[j]))
             end
          end
        end
      end
  Process.waitall
  writer.close
  while message = reader.gets
    the_pairs.push(message)
  end

  return the_pairs
end
