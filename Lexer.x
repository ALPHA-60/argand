{
module Lexer (Token(..), TokenVal(..), arglex) where
import Data.Maybe
import System.FilePath.Posix ( (</>), FilePath )
}

%wrapper "monadIOUserState"

tokens :-
<0> ^\.IS.*\n			{ begin program }
<program> \"		 { skip `andBegin` string }
<string>  \\\"           { appendToCurrString } -- XXX: Handle escape
<string>  \"		 { mkStringT `andBegin` program}
<string>  .		 { appendToCurrString }
<string>  \n		 { appendToCurrString }
<program> box            { mkT Box }
<program> var            { mkT Var }
<program> bdlist         { mkT Bdlist }
<program> boundary       { mkT Boundary }
<program> put            { mkT Put }
<program> conn           { mkT Conn }
<program> to             { mkT To }
<program> using          { mkT Using }
<program> construct      { mkT Construct }
<program> draw           { mkT Draw }
<program> opaque         { mkT Opaque }
<program> left           { mkT LeftJ }
<program> center         { mkT CenterJ }
<program> text           { mkT Text }
<program> right          { mkT RightJ }
<program> spline         { mkT Spline }
<program> at             { mkT At }
<program> interior       { mkT Interior }
<program> exterior       { mkT Exterior }
<program> [a-zA-Z][a-zA-Z0-9]* { mkIdT }
<program> [0-9]+|[0-9]*\.[0-9]+  { mkNumT }
--<program> ^\..*		 { skip }
<program> ^\.\.\.libfile 	{ skip `andBegin` library }
<library> [\ \t]+		;
<library> [^\ \t\n]+		{ mkLibT }
<library> \n			{ skip `andBegin` program }
<program> ^\.IE.*\n 		{ begin 0 }
<program> "/*"		 { skip `andBegin` comment }
<comment> "*/"		 { skip `andBegin` program }
<comment> .		 ;
<comment> \n		 ;
<program> [\ \t]+	 ;
<program> \n  		 ;
<program> "{"  		 { mkT LBrace } -- TODO: Nest it
<program> "}"  		 { mkT RBrace } -- TODO: Nest it
<program> [\<\>\(\)\[\]\+\-\*\/\=\,\;\:\.\^\~] { mkSymT }


{

lift c = Alex $ \s -> c >>= (\x -> return (Right (s,x)))

type Action = AlexInput -> Int -> Token

-- Holds filename and line number
type TokenPosn = (FilePath, Int)

data Token = Token {
    tv  :: TokenVal,
    tp  :: TokenPosn,
    str :: (Maybe String)
  } deriving (Show)

mkT tokval inp len = do
  let ((AlexPn _ lineno _), _, str) = inp
  file <- getCurrentFile
  return $ Token tokval (file, lineno) (Just (take len str))

mkLibT pos@(_, _, str) len =
  mkT (LibIncl $ take len str) pos len

mkIdT pos@(_, _, str) len =
  mkT (Id $ take len str) pos len

mkSymT pos@(_, _, str) len =
  mkT (Sym $ take len str) pos len

mkNumT pos@(_, _, str) len =
  mkT (Num $ read (take len str)) pos len

getCurrentFile = Alex $ \s ->
   return $ Right (s, currentFile (alex_ust s))

getUserState = Alex $ \s ->
  return $ Right (s, alex_ust s)

setUserState us = Alex $ \s ->
  return $ Right (s{alex_ust = us}, ())

appendToCurrString pos@(_, _, str) len = do
  us <- getUserState
  setUserState us{currentString = currentString(us) ++ (take len str)}
  skip pos len
   
  
mkStringT pos@(_, _, str) len = do
  us <- getUserState
  setUserState us{currentString = ""}
  mkT (String $ (currentString us)) pos len

   

data TokenVal =
  Box
  | Var
  | Bdlist
  | Boundary
  | Put
  | Conn
  | To
  | Using
  | Construct
  | Draw
  | Opaque
  | LeftJ
  | CenterJ
  | Text
  | RightJ
  | Spline
  | At
  | Interior
  | Exterior
  | LBrace
  | RBrace
  | LibIncl String
  | String String
  | Id String
  | Sym String
  | Num Float
  | Eof
  deriving (Eq, Show)

type FileInfo = ([Char], Int)
data AlexUserState = AlexUserState {currentFile :: FilePath, currentString :: String }

libraryPath :: FilePath
libraryPath = "."

alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) = Alex (\s -> do
	res <- al s
	case res of
          Right (s', x) -> return $ Right (s', (x, Nothing))
          Left  message -> return $ Right (s, (undefined, Just (message ++ "["  ++ currentFile (alex_ust s) ++ "]" ++ alex_inp s))))


alexEOF :: Alex Token

alexEOF = return $ Token Eof undefined Nothing

scanner = do
	(token, message) <- alexComplementError alexMonadScan
	case message of
	  (Just message) -> (alexError message)
          _ -> case token of
	        Token Eof _ _ -> return []
		Token (LibIncl file) _ _ -> do
			input <- lift $ readFile file
		  	l <- lift $ runAlex (AlexUserState { currentFile = file, currentString = "" }) input scanner
			case l of
				Left m -> (alexError m)
				Right o -> do 
					z <- scanner
					return $ o ++ z
                _ ->  do z <- scanner
	                 return (token:z)

arglex filename = do
	let alexInitUserState = AlexUserState { currentFile = filename, currentString = "" }
	input <- readFile (currentFile alexInitUserState)
	x <- (runAlex alexInitUserState input scanner)
	return x
}


