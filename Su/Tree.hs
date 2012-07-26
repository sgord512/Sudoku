module Su.Tree where

import Su.Base
import Text.PrettyPrint
import Util.Display

treeDoc :: Tree -> Doc
treeDoc Completed = text "Completed!"
treeDoc DeadEnd = text "DeadEnd"
treeDoc (Node l branches) = (text $ display l) <> 
                            space <> 
                            (nest 1 $ vcat $ map ((char '|' <> arrow <+>) . branchDoc) branches)

arrow :: Doc
arrow = text "->"

branchDoc :: Branch -> Doc
branchDoc (n, tree) = text (show n) <> colon <+> treeDoc tree

instance Display Tree where
  display = render . treeDoc


