
module Embed

bootstrapFileMap :: Map FilePath B.ByteString
bootstrapFileMap = M.fromList $(embedDir "embedded/bootstrap")
