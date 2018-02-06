module Main where

import           Universum            hiding (find, lines)

import           Data.Containers
import           Data.Sequences       hiding (find)
import           Options.Applicative
import           System.FilePath
import           System.FilePath.Find
import           System.Process


main :: IO ()
main = do
  argv <- execParser options'

  when (upgrade argv) $ do
    channels <- takeFileName . takeDirectory <<$>>
                find (pure True)
                     (fileName ==? ".update-on-nixos-rebuild")
                     "/nix/var/nix/profiles/per-user/root/channels/"

    out :: Map String (TVar (Seq (Line String))) <-
      map mapFromList . forM ("nixos" : channels) $ \channel -> do
        tvar <- newTVarIO mempty
        (ec,out,err) <- readCreateProcessWithExitCode (proc "nix-channel" ["--update", channel]) mempty
        traverse (\line -> atomically $ modifyTVar' tvar (`snoc` line)) . map Stderr . lines $ err
        pure (channel, tvar)

    whenJust (lookup "nixos" out) $ \tvar ->
       print =<< atomically (readTVar tvar <* writeTVar tvar mempty)

  exitSuccess


data Action = Switch
            | Boot
            | Test
            | Build
            deriving (Show)

data Line t = Stdout t
            | Stderr t
            deriving (Show)


readAction :: (MonadThrow m) => String -> m Action
readAction t = case toLower t of
  "switch" -> pure Switch
  "boot"   -> pure Boot
  "test"   -> pure Test
  "build"  -> pure Build
  _        -> throwM $ ReadActionException "unknown action"

newtype ReadActionException = ReadActionException Text deriving (Show)
instance Exception ReadActionException


-- COMMAND LINE ARGUMENTS

data Options = Options { action  :: Action
                       , upgrade :: Bool
                       , dryrun  :: Bool
                       } deriving (Show)

options :: Parser Options
options = do
  action <- argument (maybeReader readAction) (metavar "ACTION")
  upgrade <- switch (long "upgrade" <> help "update Nix channels first")
  dryrun <- switch (long "dryrun" <> short 'n' <> help "Show version")

  pure Options{..}

options' = info (options <**> helper)
  (fullDesc <> progDesc "reconfigure a NixOS machine" <> header "nixtools-rebuild")

