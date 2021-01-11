/*
Base on part three, an implementation in scala is shown here. 
According to part 3, the integer here is a List and the rule is a List of List(lambda expression) as well.
As an example, the rule list is "val r=List(List((i:Int) => (i % 3==0),(i:Int) => "fizz"),List((i:Int) => (i % 5==0),(i:Int) => "buzz"))" based on the scala characteristic
And the output is a List of String, which shows each element in the list straightforwardly
*/

def zuzzer(l:List[Int],rule:List[List[Int=>Any]]):List[String] = {
    var result = List[String]()

    // loop through integer list
    for (element <- l) {
        // the value to be added to the result list
        var tmp=element.toString()
        // loop through lambda expressions list
        for (rules <- rule) {
            // if the condition is met
            if (rules.head(element)==true){
                // check if tmp is already assigned another string(initially a string of int) by checking if every char in tmp is digit, if not, replace tmp
                if (tmp.forall(_.isDigit)){
                    tmp=rules(1)(element).toString()
                }
                // else, add string to the end of tmp
                else {
                    tmp=tmp+rules(1)(element)
                }

            }
            // if the condition is not met, the value to be added to result list remain the same(integer string), 
            // and loop through the next list of lambda exprssion
            else {
                tmp=tmp
            }
        }
        result=result:::List(tmp)
    }
    return result
}
