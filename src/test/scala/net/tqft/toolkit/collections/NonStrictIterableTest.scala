package net.tqft.toolkit.collections

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
 
@RunWith(classOf[JUnitRunner])
class NonStrictIterableTest extends FlatSpec with ShouldMatchers {
	
	val counting = NonStrictIterable.iterate(0)(_+1)
	
	"A NonStrictIterable" should "not force unnecessarily!" in {
		(counting map { _ + 1 } take 5).toList should equal (List(1,2,3,4,5))
	}
	
	"A NonStrictIterable" should "implement flatMap correctly" in {
		(counting flatMap { i => List(i,i) } take 3).toList should equal (List(0,0,1))	
	}
	
	"A NonStrictIterable" should "implement flatten correctly" in {
		(counting map { i => List(i,i) }).flatten.take(3) should equal (List(0,0,1))	
	}
	
}