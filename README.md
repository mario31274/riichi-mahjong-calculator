# Haskell-riichi-mahjong-hand-calculator

A Japanese Mahjong hand score calculator written in Haskell.

## Prerequisite

Has [GHCup](https://www.haskell.org/ghcup/install/) installed in system. GHC 9.4.7 recommended.

## Usage

1. Start the calculator with ```stack run```

2. Enter the hand that you want to calculate in `spmz` format (case sensitive), with each different character representing different suits.

    ```txt
    s = Banboos (Sou)
    p = Circles (Pin)
    m = Characters (Man)
    z = Honors (Jihai)
    ```

    For example, a Seven Pair hand

    ```txt
    🀚,🀚,🀛,🀛,🀜,🀜,🀔,🀔,🀕,🀕,🀖,🀖,🀎,🀎
    ```

    can be parsed by this line

    ```haskell
    "223344p556677s88m"
    ```

    The order of the suits can be in any order as user wishes.

3. The parser also supports open melds (Chi, Pon, or Kan) in the following format:

   ```txt
    C = Chi (Run)
    P = Pon (Triplet)
    K = Kan (Quad), opened
    k = Kan (Quad), closed
    ```

    The melds should be in the format like `C?s`

    ```txt
    C4p = Chi (🀜🀝🀞)
    P5z = Pon (🀁🀁🀁)
    ```

    It does not matter how many digits is in between the meld type and the suit. The parser will only look at the digit adjacent to the meld type, allowing user entering `C456p` and still getting the Chi meld 🀜🀝🀞.

    You can separate the hand and melds with non-alphanumeric characters like `#`, but it's optional.

    For example, the any of the below inputs

    * `45678p123s88m3p#C456p`

    * `45678p123s88m3pC456p`

    * `45678p123s88m3pC4p`

    will produce

    ```txt
    [🀜,🀝,🀞,🀟,🀠,🀐,🀑,🀒,🀎,🀎,🀛],[Chi(🀜,🀝,🀞)]
    ```

    **For a correct calculation, the winning should put behind all closed tiles and in front of the open melds.** The example above means that `3 Pin` is the winning tile.

4. Answer for a few more questions that are necessary for score calculation.

    ```txt
    What's the Round Wind? (e/s/w/n)

    What's the Seat Wind? (e/s/w/n)

    Is this a Tsumo hand (self-picked hand)? (y/n)

    How many Doras (Bonus tiles) are in this hand?

    Is Riichi? (s = Single Riichi, d = Double Riichi, Default=No)?

    Is Ippatsu (one-shot)? (y/n) (<-- Only prompt when Riichi'd)

    Is there one of the following rare conditions? (Default = 0)
    0 = None of the below
    1 = DeadWallDraw  5 = NagashiMangan
    2 = RobbingAQuad  6 = BlessingOfHeave
    3 = UnderTheSea   7 = BlessingOfEarth
    4 = UnderTheRiver 8 = BlessingOfMan
    ```

    Press `Enter` and see the result!

    It will list all results in the ascending order, so you should look at the last one for the highest point.

5. Enter `quit` at any time to terminate the calculator.

6. Sample output:

    ```txt
    Enter the hand you want to calculate:
       1　2  3  4　 5  6  7　8  9   Cns = Chi (Run) meld
    s  🀐　🀑　🀒　🀓　🀔　🀕　🀖　🀗　🀘   Pns = Pon (Triplet) meld
    p  🀙　🀚　🀛　🀜　🀝　🀞　🀟　🀠　🀡   Kns = Open Kan (Quad) meld
    m  🀇　🀈　🀉　🀊　🀋　🀌　🀍　🀎　🀏   kns = Closed Kan meld
    z 　　🀀　🀁　🀂　🀃　🀆　🀅 🀄      * Must put the melds behind all tiles
          1　2  3  4  5  6  7
    23455667788pC2p                       
    What's the Round Wind? (e/s/w/n)
    e
    What's the Seat Wind? (e/s/w/n)
    e
    Is this a Tsumo hand (self-draw tile)? (y/n)
    n
    How many Doras (Bonus tiles) are in this hand?
    0
    Riichi hand? (y = Yes, Single Riichi, d = Double Riichi, Default=No)
    [Enter]
    Is there one of the following rare conditions? (Default = 0)
    0 = None of the below
    1 = DeadWallDraw  5 = NagashiMangan
    2 = RobbingAQuad  6 = BlessingOfHeave
    3 = UnderTheSea   7 = BlessingOfEarth
    4 = UnderTheRiver 8 = BlessingOfMan
    [Enter]
    You Entered:
    ([🀚,🀛,🀜,🀝,🀝,🀞,🀞,🀟,🀟,🀠,🀠],[Chi(🀚,🀛,🀜)])
    -=-=-=-=-=-=-=-=-=-Result-=-=-=-=-=-=-=-=-=-
    (🀚,🀛,🀜) (🀝,🀞,🀟) (🀝,🀞,🀟) (🀠,🀠) Chi(🀚,🀛,🀜)
    1 Han / 30 Fu                  1500 pts
    ------------         Fu         ------------
    Base  20
    Single Tile Wait  2
    ------------        Yaku        ------------
    All Simple    1
    -=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-

    -=-=-=-=-=-=-=-=-=-Result-=-=-=-=-=-=-=-=-=-
    (🀚,🀛,🀜) (🀞,🀟,🀠) (🀞,🀟,🀠) (🀝,🀝) Chi(🀚,🀛,🀜)
    2 Han / 30 Fu                  2900 pts
    ------------         Fu         ------------
    Base  20
    Closed Ron  10
    ------------        Yaku        ------------
    All Simple    1
    No Points Hand    1
    -=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-

    Press Enter to calculate next hand.
    quit
    Goodbye.
    ```

7. Lastly, you can use ```stack test``` to run tests

    ```bash
    stack test
    ```

## Encountered Wrong Results/Bugs?

Open an issue on GitHub repo with the erroneous output. Will get it fixed as soon as possible.
