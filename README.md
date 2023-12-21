# Haskell-riichi-mahjong

A WIP Japanese Mahjong Game written in Haskell

The base game is still in progress. The hand calculator is halfway finished
and supports string parsing and meld splitting. Score calculation is on its
way.

# Usage

1. Change the string in `main :: IO` to what you want to calculate in `s,p,m,z` format, with each different character representing different suits (case sensitive).

    ```txt
    s = Banboos (Sou)
    p = Circles (Pin)
    m = Characters (Man)
    z = Honors (Jihai)
    ```

    For example, a Seven Pair hand

   ```txt
   🀚🀚🀛🀛🀜🀜🀔🀔🀕🀕🀖🀖🀎🀎
   ```

   can be parsed by this line

   ```haskell
   let hand = parse "223344p556677s88m"
   ```

   The order of the suits can be in any order as user wishes.

2. The parser also supports open melds (Chi, Pon, or Kan) in the following format:

   ```txt
    C = Chi (Run)
    P = Pon (Triplet)
    K = Kan (Quad), opened
    k = Kan (Quad), closed
    ```

    The melds should be in the format like `C?s`

    ```
    C4p = Chi (🀜🀝🀞)
    P5z = Pon (🀁🀁🀁)
    ```

    It does not matter how many digits is in between the meld type and the suit. The parser will only look at the digit adjacent to the meld type, allowing user entering `C456p` and still getting the Chi meld 🀜🀝🀞.

    You can separate the hand and melds with non-alphanumeric characters like `#`, but it's optional.

    For example, the input

    ```txt
    45678p123s88m3p#C456p
    ```

    will produce

    ```txt
    [🀜,🀝,🀞,🀟,🀠,🀐,🀑,🀒,🀎,🀎,🀛],[Chi(🀜,🀝,🀞)]
    ```

3. Finally, use ```stack run``` to start the main procedure (not quite finished yet)

    ```bash
    stack run
    ```

4. You can use ```stack test``` to run tests

    ```bash
    stack test
    ```
