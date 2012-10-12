package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.types._
import util.TypeTest
import ftanml.grammar.{ChoiceParticle, LeafParticle, SequenceParticle}

class GrammarTest extends FlatSpec with TypeTest {
  
  "Simple sequences" should "match the grammar" in {
    FtanArray(List(FtanNumber(1), FtanString("a"))) ==>
      new SequenceParticle(1, 1, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
    FtanArray(List(FtanNumber(1), FtanNumber(1))) !=>
      new SequenceParticle(1, 1, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
    FtanArray(List(FtanNumber(1), FtanString("a"), FtanNumber(1))) !=>
      new SequenceParticle(1, 1, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
    FtanArray(List(FtanNumber(1))) !=>
      new SequenceParticle(1, 1, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
    FtanArray(List()) ==>
      new SequenceParticle(1, 1, List()).makeGrammar
  }

  "Simple choices" should "match the grammar" in {
    FtanArray(List(FtanNumber(1))) ==>
      new ChoiceParticle(1, 1, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
    FtanArray(List(FtanString("a"))) ==>
      new ChoiceParticle(1, 1, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
    FtanArray(List(FtanTrue)) !=>
      new ChoiceParticle(1, 1, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
    FtanArray(List()) !=>
      new ChoiceParticle(1, 1, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
  }

  "Repetitions" should "match the grammar" in {
    FtanArray(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(3, 3, List(new LeafParticle(NumberType))).makeGrammar
    FtanArray(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(3, 4, List(new LeafParticle(NumberType))).makeGrammar
    FtanArray(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(1, 3, List(new LeafParticle(NumberType))).makeGrammar
    FtanArray(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(1, -1, List(new LeafParticle(NumberType))).makeGrammar
    FtanArray(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(0, -1, List(new LeafParticle(NumberType))).makeGrammar
    FtanArray(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(3, -1, List(new LeafParticle(NumberType))).makeGrammar
    FtanArray(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) !=>
      new SequenceParticle(4, -1, List(new LeafParticle(NumberType))).makeGrammar
  }

  "Repeated choices" should "match the grammar" in {
    FtanArray(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new ChoiceParticle(1, 3, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
    FtanArray(List(FtanString("a"), FtanString("b"), FtanNumber(3))) ==>
      new ChoiceParticle(3, 5, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
    FtanArray(List(FtanString("a"), FtanString("b"), FtanNumber(3))) ==>
      new ChoiceParticle(3, -1, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
    FtanArray(List(FtanString("a"), FtanString("b"), FtanNumber(3))) !=>
      new ChoiceParticle(4, -1, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
    FtanArray(List(FtanString("a"), FtanString("b"), FtanNumber(3), FtanTrue)) !=>
      new ChoiceParticle(4, -1, List(new LeafParticle(NumberType), new LeafParticle(StringType))).makeGrammar
  }

  "Ambiguous choices" should "match the grammar" in {
    FtanArray(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new ChoiceParticle(1, 3, List(new LeafParticle(new MaxValueType(FtanNumber(3), false)),
                                    new LeafParticle(new MinValueType(FtanNumber(1), false)))).makeGrammar
    FtanArray(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(1, 1,
        List(new ChoiceParticle(1, 1, List(new LeafParticle(new MaxValueType(FtanNumber(3), false)),
                                    new LeafParticle(new MinValueType(FtanNumber(1), false)))),
             new LeafParticle(NumberType),
             new LeafParticle(NumberType))).makeGrammar
    FtanArray(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(1, 1,
        List(new ChoiceParticle(1, 3, List(new LeafParticle(new MaxValueType(FtanNumber(3), false)),
                                    new LeafParticle(new MinValueType(FtanNumber(1), false)))),
             new SequenceParticle(0, 2, List(new LeafParticle(NumberType))))).makeGrammar
  }

}