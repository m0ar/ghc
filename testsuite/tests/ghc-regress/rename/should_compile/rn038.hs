-- !!! it is legal to import a module hiding an entity that it doesn't export
module ShouldCompile where
import List hiding ( wibble )
