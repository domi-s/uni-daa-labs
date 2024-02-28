/** Import is for readLine so that we can write input directly to the program */
import scala.io.StdIn

object Brack{
	//Maximum length of word so we can define our arrays in dynamic programming
	val MAXWORD = 30

	//Operation to take 'A', 'B' and 'C' to corresponding Ints
  def LetterToInt(a: Char) : Int = {
		if(a == 'A' || a == 'B' || a == 'C'){
			return (a.toInt - 'A'.toInt);
		} else{
			println("Please only Letters from A,B,C.")
			sys.exit
		}
	}
	
  //Defining the op array for everything to use
  val op = Array.ofDim[Int](3,3)  
  op(0)(0) = 1; op(0)(1) = 1; op(0)(2) = 0
	op(1)(0) = 2; op(1)(1) = 1; op(1)(2) = 0
	op(2)(0) = 0; op(2)(1) = 2; op(2)(2) = 2

  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray.init

 
  /* Functions below here need to be implemented */


	//TASK 1
	//PossibleRec checks whether bracketing to something is possible recursively
	//Checks whether w(i,j) can be bracketed to z
	
	def PossibleRec(w: Array[Int], i: Int, j: Int, z:Int): Boolean = {
		if(i + 1 == j) return w(i) == z
		
		var ans = false
		for(k <- i + 1 until j)
			for(x <- 0 to 2)
				for(y <- 0 to 2)
					if(op(x)(y) == z)
						ans = ans || (PossibleRec(w, i, k, x) && PossibleRec(w, k, j, y))
		ans
	} 

	
	//TASK 2
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w(i,j) can be bracketed to get z
	
	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
		if(i + 1 == j) return if (w(i) == z) 1 else 0
		
		var ans = 0
		for(k <- i + 1 until j)
			for(x <- 0 to 2)
				for(y <- 0 to 2)
					if(op(x)(y) == z)
						ans += NumberRec(w, i, k, x) * NumberRec(w, k, j, y)
		ans
	} 

	
	//TASK 3
	//TODO Runtime analysis of recursive solution along with tests
	/*
		A lot of time (both functions iterate the same)
		For a (sub)string of length n, we iterate with k through the possible places to split it, in linear time.
		But this process continues recursively, and we happen to recalculate the same answers a bunch of times.
		For example, for w = BBBBBB, we might split it into (BB)(BBBB) and then later on into (BBB)(BBBB), and inside the second split we might reach (BB)(B)(BBBB), meaning that we recalculate the first (BB).

		Unlike setting w equal a random number of A's, B's and C's, by setting all the letters equal, we can properly see that we recalculate a ton of values. As such, for example, setting w = BBBBBBBBBBBBBBBBBBBBBBBBBBBBB (30 B's), we get the following results:

		tw69[~/DAA]$ time scala Brack -NumberRec testcase
		Bracketing values for BBBBBBBBBBB
		A can be achieved in 0 ways
		B can be achieved in 16796 ways
		C can be achieved in 0 ways

		real    0m8.692s
		user    0m8.891s
		sys     0m0.097s

		Looping through x and y we get 9 possible pairs of values, but only 6 of them will equal the wanted value z. So,

		T(1) = 1
		T(2) = 6T(1)
		...
		T(n) = 3(T(n - 1) + T(1)) + 3(T(n - 2) + T(2)) + ... + 3(T(1) + T(n - 1))
		     = 6T(n - 1) + 6T(n - 2) + ... + 6T(2) + 6T(1)
			 = 6T(1) + 6T(2) + ... + 6T(n - 2) + 6T(n - 1)

		So,
		T(n) = 6T(1) + 6^2T(1) + ... + 6^(n - 2)T(1) + 6^(n - 1)T(1)
		     = 6 + 6^2 + ... + 6^(n - 1) + 1 - 1
			 = ((6^n - 1) / (6 - 1)) - 1
			 = ((6^n - 1) / 5) - 1.
			
		So the time complexity is O(6^n).

		TODO TESTING
	*/
	
	//You may find the following class useful for Task 7
	// Binary tree class
	abstract class BinaryTree
	case class Node (left : BinaryTree, right : BinaryTree) extends BinaryTree
	case class Leaf (value : Char) extends BinaryTree

	//Printing for a binary tree
	def print_tree(t : BinaryTree): Unit {
	//TODO(optional)
	}

	//These arrays should hold the relevant data for dynamic programming
	var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
	var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
	var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)


	//Task 4, 5, and 7(optional)
	//TODO Fill out arrays with dynamic programming solution
	
	
	def TabulatePoss(w: Array[Int], n: Int): Unit = {
		//Base case, as with the recursive implementation
		for(i <- 0 until n)
			for(z <- 0 to 2)
				poss(i)(i + 1)(z) = w(i) == z
		
		//Recurrence
		for(ln <- 2 to n)
			for(i <- 0 to n - ln) {
				val j = i + ln
				for(k <- i + 1 until j)
					for(x <- 0 to 2) for(y <- 0 to 2)
						poss(i)(j)(op(x)(y)) |= (poss(i)(k)(x) && poss(k)(j)(y))
			}
	}
	
	
	def Tabulate(w: Array[Int], n: Int): Unit = {
		//Base case, as with the recursive implementation
		for(i <- 0 until n)
			for(z <- 0 to 2)
				ways(i)(i + 1)(z) = if(w(i) == z) 1 else 0
		
		//Recurrence
		for(ln <- 2 to n)
			for(i <- 0 to n - ln) {
				val j = i + ln
				for(k <- i + 1 until j)
					for(x <- 0 to 2) for(y <- 0 to 2)
						ways(i)(j)(op(x)(y)) += ways(i)(k)(x) * ways(k)(j)(y)
			}
	}

	//Task 6
	/*
		
	*/
  

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {

    // string to print if error occurs
    val errString = 
      "Usage: scala Brack -PossibleRec [file]\n"+
      "     | scala Brack -NumberRec [file]\n"+
      "     | scala Brack -Tabulate [file]\n"
		
		if (args.length > 2){
			println(errString)
			sys.exit
		}

    //Get the plaintext, either from the file whose name appears in position
    //pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else StdIn.readLine.toArray

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
		val plain = getPlain(1)
    val command = args(0)

		//Making sure the letters are of the right type
		val len = plain.length
		var plainInt = new Array[Int](len)
		if (len > MAXWORD){
			println("Word Too Long! Change MAXWORD")
			sys.exit;
		} else {
    	for (i <- 0 until len){
				plainInt(i) = LetterToInt(plain(i))
			}
		}
		
		//Executing appropriate command
    if(command=="-PossibleRec"){
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			if(PossibleRec(plainInt, 0, len, i)){
				println(('A'.toInt + i).toChar + " is Possible");
			}
			else{
				println(('A'.toInt + i).toChar + " is not Possible");
			}
		}
    }
    else if(command=="-NumberRec"){
		var z: Int = 0
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			z = NumberRec(plainInt, 0, len, i)
			if(z == 1){
				printf(('A'.toInt + i).toChar+ " can be achieved in %d way\n", z)
			}
			else{
				printf(('A'.toInt + i).toChar+ " can be achieved in %d ways\n", z)
			}
		}
    }

    else if(command=="-Tabulate"){
		Tabulate(plainInt,len)
		println("Bracketing values for "+ plain.mkString(""))
		for(v<-0 to 2){
		var z: Int = ways(0)(len)(v)
			if(z==0){
			println(('A'.toInt + v).toChar+ " cannot be achieved")
			}
			else if(z==1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d way\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
			else if (z > 1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d ways\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
		}
    }      
    else println(errString)
  }
}


