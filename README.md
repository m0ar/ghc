The Glasgow Haskell Compiler
============================

Special building instructions:

1. Clone the GHC repository mirror from GitHub: 

  ```
  $ git clone https://github.com/ghc/ghc
  ```

2. Because of differing naming conventions in GHC and GitHub we need to give `git` some extra pointers:

  ```
  $ git config --global url."git://github.com/ghc/packages-".insteadOf     git://github.com/ghc/packages/
  $ git config --global url."http://github.com/ghc/packages-".insteadOf    http://github.com/ghc/packages/
  $ git config --global url."https://github.com/ghc/packages-".insteadOf   https://github.com/ghc/packages/
  $ git config --global url."ssh://git\@github.com/ghc/packages-".insteadOf ssh://git\@github.com/ghc/packages/
  $ git config --global url."git\@github.com:/ghc/packages-".insteadOf      git\@github.com:/ghc/packages/
  ```
  
3. Update and initialise all the submodules:

  ```
  $ cd ghc
  $ git submodule update --init
  ```

4. Add this branch as a remote source:

  ```
  $ git remote add m0ar https://github.com/m0ar/ghc.git
  $ git fetch m0ar improved-ado
  $ git checkout improved-ado
  ```

5. Create a `mk/build.mk` (see `mk/build.mk.sample`)

6. Then setup the project for building:
  ```
  $ ./boot
  $ ./configure
  $ make -j{# physical cores + 1}
  ```
