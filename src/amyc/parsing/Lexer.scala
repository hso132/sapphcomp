package amyc
package parsing

import utils._
import scala.io.Source
import java.io.File
import scala.collection.immutable.Stream;

// The lexer for Amy.
// Transforms an iterator coming from scala.io.Source to a stream of (Char, Position),
// then uses a functional approach to consume the stream.
object Lexer extends Pipeline[List[File], Stream[Token]] {
  import Tokens._

  /** Maps a string s to the corresponding keyword,
   * or None if it corresponds to no keyword
   */
  private def keywords(s: String): Option[Token] = s match {
    case "abstract" => Some(ABSTRACT())
    case "Boolean"  => Some(BOOLEAN())
    case "case"     => Some(CASE())
    case "class"    => Some(CLASS())
    case "def"      => Some(DEF())
    case "else"     => Some(ELSE())
    case "error"    => Some(ERROR())
    case "extends"  => Some(EXTENDS())
    case "false"    => Some(FALSE())
    case "if"       => Some(IF())
    case "Int"      => Some(INT())
    case "match"    => Some(MATCH())
    case "object"   => Some(OBJECT())
    case "String"   => Some(STRING())
    case "true"     => Some(TRUE())
    case "Unit"     => Some(UNIT())
    case "val"      => Some(VAL())
    case _          => None
  }

  private def lexFile(ctx: Context)(f: File): Stream[Token] = {
    import ctx.reporter._

    // Special character which represents the end of an input file
    val EndOfFile: Char = scala.Char.MaxValue

    val source = Source.fromFile(f)

    // Useful type alias:
    // The input to the lexer will be a stream of characters,
    // along with their positions in the files
    type Input = (Char, Position)

    def mkPos(i: Int) = Position.fromFile(f, i)

    // The input to the lexer
    val inputStream: Stream[Input] =
      source.toStream.map(c => (c, mkPos(source.pos))) #::: Stream((EndOfFile, mkPos(source.pos)))

    /** Gets rid of whitespaces and comments and calls readToken to get the next token.
     * Returns the first token and the remaining input that did not get consumed
     */
    @scala.annotation.tailrec
    def nextToken(stream: Stream[Input]): (Token, Stream[Input]) = 
    {
      require(stream.nonEmpty)

      
      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      //get rid of whitespaces
      if (Character.isWhitespace(currentChar)) 
      {
        nextToken(stream.dropWhile{ case (c, _) => Character.isWhitespace(c) } )
      } 
      else if (currentChar == '/' && nextChar == '/') 
      {
        // Single-line comment
        nextToken(stream.dropWhile{ case (c, _) => c != '\n'})
      } 
      else if (currentChar == '/' && nextChar == '*') 
      {
        destroyComments(stream) match
        {
          case (Some(b), nuStream) => 
            ctx.reporter.error("Unclosed comment", b.position)
            (b.setPos(currentPos), nuStream);
          case (None, nuStream) => nextToken(nuStream);
        }
      }
      else
      {
        readToken(stream)
      }
    }

    //gets rid of everything until it finds "*/"
    //if it finds an end-of-file character, it returns a BAD token
    @scala.annotation.tailrec
    def destroyComments(stream: Stream[Input]): (Option[Token],Stream[Input]) =
    {
      val (currentChar, currentPos) #:: rest = stream;
      def nextChar = rest.head._1

      currentChar match
      {
        case '*' => 
          if(nextChar == '/') (None, rest.tail)
          else destroyComments(rest)
        case `EndOfFile` => (Some(BAD()), rest)
        case _ => destroyComments(rest)
      }
    }
    /** Reads the next token from the stream. Assumes no whitespace or comments at the beginning.
     * Returns the first token and the remaining input that did not get consumed.
     */
    def readToken(stream: Stream[Input]): (Token, Stream[Input]) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      // Returns input token with correct position and uses up one character of the stream
      def useOne(t: Token) = (t.setPos(currentPos), rest)
      // Returns input token with correct position and uses up two characters of the stream
      def useTwo(t: Token) = (t.setPos(currentPos), rest.tail)

      def throwOne() =
      {
        ctx.reporter.error("Invalid Token", currentPos);
        useOne(BAD())
      }
      currentChar match 
      {
        case `EndOfFile` => useOne(EOF())

        // Reserved word or Identifier
        case _ if Character.isLetter(currentChar) =>
          val (wordLetters, afterWord) = stream.span 
          { 
            case (ch, _) => Character.isLetterOrDigit(ch) || ch == '_'
          }

          val word: String = wordLetters.map(_._1).mkString
          // Hint: Decide if it's a letter or reserved word (use our infrastructure!),
          // and return the correct token, along with the remaining input stream.
          // Make sure you set the correct position for the token.
          val token = keywords(word) match 
          {
            case Some(k) => k
            case None => ID(word)
          };

          (token.setPos(currentPos), afterWord)
          // Int literal
        case _ if Character.isDigit(currentChar) =>
          // Hint: Use a strategy similar to the previous example.
          // Make sure you fail for integers that do not fit 32 bits.
          val (wordLetters, afterWord) = stream.span
          {
            case(ch, _) => Character.isDigit(ch)
          }
          val word: String = wordLetters.map(_._1).mkString;

          val token = try
          {
            val i = word.toInt;
            INTLIT(i)
          }
          catch
          {
            case e: Exception => 
              ctx.reporter.error("Invalid integer litteral", currentPos);
              BAD()
          }
          (token.setPos(currentPos), afterWord)

          // String literal
        case '"' =>
          val (wordLetters, afterWord) = rest.span{case(ch,_) => ch != '"'};
          val word = wordLetters.map(_._1).mkString;
          val (token, trueRest) = afterWord match
          {
            case _ #:: tail => 
              if(!word.contains('\n')) (STRINGLIT(word), tail)
              else 
              {
                ctx.reporter.error("Unclosed string literal", currentPos);
                (BAD(), afterWord)
              }
            case Stream.Empty => 
              ctx.reporter.error("Unclosed string literal", currentPos);
              (BAD(), Stream.Empty)
          }
          (token.setPos(currentPos), trueRest)
          

        case '.' => useOne(DOT());
        case '{' => useOne(LBRACE());
        case '}' => useOne(RBRACE());
        case '(' => useOne(LPAREN());
        case ')' => useOne(RPAREN());
        case '+' =>
          if(nextChar == '+') useTwo(CONCAT());
          else useOne(PLUS());
        case ':' => useOne(COLON());
        case ',' => useOne(COMMA());
        case '=' => 
          if(nextChar == '=') useTwo(EQUALS());
          if(nextChar == '>') useTwo(RARROW());
          else useOne(EQSIGN());

        case ';' => useOne(SEMICOLON());
        case '%' => useOne(MOD());
        case '/' => useOne(DIV());
        case '*' => useOne(TIMES());
        case '|' => 
          if(nextChar == '|') useTwo(OR());
          else throwOne;
        case '&' =>
          if(nextChar == '&') useTwo(AND());
          else throwOne;
        case '!' => useOne(BANG());
        case '-' => useOne(MINUS());
        case '<' => 
          if(nextChar == '=') useTwo(LESSEQUALS())
          else useOne(LESSTHAN());

        case '_' => useOne(UNDERSCORE());

        
        case _ => throwOne;
      }
    }

    // To lex a file, call nextToken() until it returns the empty Stream as "rest"
    def tokenStream(s: Stream[Input]): Stream[Token] = {
      if (s.isEmpty) Stream()
      else {
        val (token, rest) = nextToken(s)
        token #:: tokenStream(rest)
      }
    }

    tokenStream(inputStream)
  }

  // Lexing all input files means putting the tokens from each file one after the other
  def run(ctx: Context)(files: List[File]): Stream[Token] = {
    files.toStream flatMap lexFile(ctx)
  }
}

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Stream[Token], Unit] {
  def run(ctx: Context)(tokens: Stream[Token]): Unit = {
    tokens.toList foreach { t => println(s"$t(${t.position.withoutFile})") }
  }
}
