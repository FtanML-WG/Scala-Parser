package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.types._
import util.TypeTest
import ftanml.grammar.{ChoiceParticle, LeafParticle, SequenceParticle}

class GrammarTest extends FlatSpec with TypeTest {
  
  "Simple sequences" should "match the grammar" in {
    FtanList(List(FtanNumber(1), FtanString("a"))) ==>
      new SequenceParticle(1, 1, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
    FtanList(List(FtanNumber(1), FtanNumber(1))) !=>
      new SequenceParticle(1, 1, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
    FtanList(List(FtanNumber(1), FtanString("a"), FtanNumber(1))) !=>
      new SequenceParticle(1, 1, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
    FtanList(List(FtanNumber(1))) !=>
      new SequenceParticle(1, 1, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
    FtanList(List()) ==>
      new SequenceParticle(1, 1, List()).makeGrammar
  }

  "Simple choices" should "match the grammar" in {
    FtanList(List(FtanNumber(1))) ==>
      new ChoiceParticle(1, 1, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
    FtanList(List(FtanString("a"))) ==>
      new ChoiceParticle(1, 1, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
    FtanList(List(FtanTrue)) !=>
      new ChoiceParticle(1, 1, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
    FtanList(List()) !=>
      new ChoiceParticle(1, 1, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
  }

  "Repetitions" should "match the grammar" in {
    FtanList(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(3, 3, List(LeafParticle(NumberType))).makeGrammar
    FtanList(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(3, 4, List(LeafParticle(NumberType))).makeGrammar
    FtanList(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(1, 3, List(LeafParticle(NumberType))).makeGrammar
    FtanList(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(1, -1, List(LeafParticle(NumberType))).makeGrammar
    FtanList(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(0, -1, List(LeafParticle(NumberType))).makeGrammar
    FtanList(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(3, -1, List(LeafParticle(NumberType))).makeGrammar
    FtanList(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) !=>
      new SequenceParticle(4, -1, List(LeafParticle(NumberType))).makeGrammar
  }

  "Repeated choices" should "match the grammar" in {
    FtanList(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new ChoiceParticle(1, 3, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
    FtanList(List(FtanString("a"), FtanString("b"), FtanNumber(3))) ==>
      new ChoiceParticle(3, 5, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
    FtanList(List(FtanString("a"), FtanString("b"), FtanNumber(3))) ==>
      new ChoiceParticle(3, -1, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
    FtanList(List(FtanString("a"), FtanString("b"), FtanNumber(3))) !=>
      new ChoiceParticle(4, -1, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
    FtanList(List(FtanString("a"), FtanString("b"), FtanNumber(3), FtanTrue)) !=>
      new ChoiceParticle(4, -1, List(LeafParticle(NumberType), LeafParticle(StringType))).makeGrammar
  }

  "Ambiguous choices" should "match the grammar" in {
    FtanList(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new ChoiceParticle(1, 3, List(LeafParticle(new MaxValueType(FtanNumber(3), false)),
                                    LeafParticle(new MinValueType(FtanNumber(1), false)))).makeGrammar
    FtanList(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(1, 1,
        List(new ChoiceParticle(1, 1, List(LeafParticle(new MaxValueType(FtanNumber(3), false)),
                                    LeafParticle(new MinValueType(FtanNumber(1), false)))),
             LeafParticle(NumberType),
             LeafParticle(NumberType))).makeGrammar
    FtanList(List(FtanNumber(1), FtanNumber(2), FtanNumber(3))) ==>
      new SequenceParticle(1, 1,
        List(new ChoiceParticle(1, 3, List(LeafParticle(new MaxValueType(FtanNumber(3), false)),
                                    LeafParticle(new MinValueType(FtanNumber(1), false)))),
             new SequenceParticle(0, 2, List(LeafParticle(NumberType))))).makeGrammar
  }

}