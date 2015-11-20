module Opts
    ( parseActions
    ) where


import           Control.Error
import           Options.Applicative

import           Types


parseActions :: Script Actions
parseActions = scriptIO $ execParser actions


actions :: ParserInfo Actions
actions = info (helper <*> actions')
          (  fullDesc
          <> progDesc "A program for working with topic models."
          <> header "topical - A program for working with topic models.")

actions' :: Parser Actions
actions' = subparser
           (  command "load"
              (info (helper <*> loadAction)
                        (  fullDesc
                        <> progDesc "Load a corpus into a vector space model."))
           )

loadAction :: Parser Actions
loadAction = LoadCorpus
             <$> strOption (  short 'i' <> long "input" <> metavar "DIRECTORY"
                           <> help "The directory to process into a file.")
             <*> optional (strOption (  short 's' <> long "stopwords"
                                     <> metavar "FILENAME"
                                     <> help "A file to read for stop words."))
             <*> strOption (  short 'o' <> long "output" <> metavar "FILENAME"
                           <> help "A file to write the corpus into.")
