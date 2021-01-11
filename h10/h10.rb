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
require 'date'
require_relative 'collection'



puts (summingPairs(INPUT,2020))
# puts "Starting at:   #{DateTime.now.sec} seconds, #{DateTime.now.strftime("%9N")} nanoseconds"
puts (summingPairs(INPUT,2020).include?(Pair.new(626, 1030)))
# puts "Ending at:     #{DateTime.now.sec} seconds, #{DateTime.now.strftime("%9N")} nanoseconds"
