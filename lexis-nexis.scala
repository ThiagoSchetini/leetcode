// Scala


// 1 Valid Anagram
def isAnagram(s: String, t: String): Boolean = {
	s.toArray.sorted.sameElements(t.toArray.sorted)
}


// 2 Longest Common Prefix O(n)
def longestCommonPrefix(strs: Array[String]): String = {

	var prefix = ""

	if (strs.length == 0) {
	  prefix

	} else if (strs.length == 1) {
	  strs(0)

	} else {
	  val sortedArray = strs.sorted
	  val minLength = sortedArray.reduceLeft((a,b) => if (a.length < b.length) a else b).length
	  val firstWord = sortedArray.head
	  val lastWord = sortedArray.last

	  // comparing only first and last words of a sorted array solves the problem
	  var flagBreak = false

	  for (i <- 0 until minLength) {
			if ( !flagBreak && (firstWord(i) == lastWord(i)) ) {
				prefix += firstWord(i)
			} else {
				flagBreak = true
			}
	  }

	  prefix
	}
}


// 3 First Bad Version O(log n)     
def firstBadVersion(n: Int): Int = {
	var head = 1
	var last = n

	while (head != last) {
		val middle = head + ((last - head) / 2)
		if (isBadVersion(middle)) {
			last = middle
		} else {
			head = middle + 1
		}
	}

	head
}


// 4 Letter Combinations of a Phone Number
def letterCombinations(digits: String): List[String] = {

	val phoneKeys = Map(2 -> Array("a","b","c"), 3 -> Array("d","e","f"), 4 -> Array("g","h","i"), 5 -> Array("j","k","l"),
		6 -> Array("m","n","o"), 7 -> Array("p","q","r","s"), 8 -> Array("t","u","v"), 9 -> Array("w","x","y","z"))

	val selectKeys = digits.toArray.map(_.getNumericValue).map(phoneKeys(_))

	if(selectKeys.length == 1) {
		selectKeys.head.toList

	} else if (!selectKeys.isEmpty) {

		selectKeys.reduce((left, right) => combine(left, right)).toList

	} else {
		List()
	}
}


def combine(left: Array[String], right: Array[String]): Array[String] = {

	left.flatMap(l => {
		right.map(r => String.valueOf(l) + String.valueOf(r))
	})
}
	
	
// 5 Top K Frequent
def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {

	// O(1)
	if(k == nums.length) {
	  nums

	// O(n)
	} else {
	  nums
		.groupBy(n => n)
		.mapValues(_.length)
		.toSeq.sortWith(_._2 > _._2)
		.take(k).map(_._1).toArray.sorted
	}
}
	
	
