#!/usr/bin/env runhaskell

> import Distribution.Simple
> import Distribution.Simple.PreProcess
> import Distribution.Simple.Program
> import Distribution.Simple.LocalBuildInfo

> pp :: LocalBuildInfo -> Program -> [String] -> PreProcessor
> pp lbi prog args =
>   PreProcessor {
>     platformIndependent = True , 
>     runPreProcessor = mkSimplePreProcessor $ \inf outf verb ->
>       rawSystemProgramConf verb prog (withPrograms lbi)
>         (args ++ ["-o", outf, inf])
>   }

> ppAlex' :: [String] -> a -> LocalBuildInfo -> PreProcessor
> ppAlex' args _ lbi = pp lbi alexProgram args

> main = defaultMainWithHooks simpleUserHooks {
>         hookedPreProcessors = [("x", ppAlex' ["-g", "-t", "alex"])]
>       }

