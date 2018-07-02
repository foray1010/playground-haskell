# Homework2

## How to test with `error.log` or `sample.log`

### with stack

1. just run `stack ghci` in shell

1. run test command such as

    ```hs
    testParse parse 10 "./src/Homework2/error.log"
    ```

### without stack

1. open `ghci` in shell

    ```sh
    ghci
    ```

1. load `Homework2.hs` and `Log.hs`

    ```hs
    -- import modules
    :l src/Homework2/Log.hs src/Homework2/Homework2.hs
    -- as only the first module from `:l` command will be executed
    -- also execute `Homework2.Homework2` here
    :m + Homework2.Homework2
    ```
