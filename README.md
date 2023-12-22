# Haskell-riichi-mahjong

A WIP Japanese Mahjong Game written in Haskell

The base game is still in progress. The hand calculator is halfway finished
and supports string parsing and meld splitting. Score calculation is on its
way.

# Usage

1. Start the calculator with ```stack run```

2. Enter the hand that you want to calculate in `s,p,m,z` format, with each different character representing different suits (case sensitive).

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

    For example, the input

    ```haskell
    "45678p123s88m3p#C456p"
    ```

    will produce

    ```txt
    [🀜,🀝,🀞,🀟,🀠,🀐,🀑,🀒,🀎,🀎,🀛],[Chi(🀜,🀝,🀞)]
    ```

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

5. Sample output:

    ```txt
    Enter the hand you want to calculate:
    23455667788pC2p
    What's the Round Wind? (e/s/w/n)
    e
    What's the Seat Wind? (e/s/w/n)
    e
    Is this a Tsumo hand (self-picked hand)? (y/n)
    n
    How many Doras (Bonus tiles) are in this hand?
    0
    Is Riichi? (s = Single Riichi, d = Double Riichi, Default=No)?

    Is there one of the following rare conditions? (Default = 0)
    0 = None of the below
    1 = DeadWallDraw  5 = NagashiMangan
    2 = RobbingAQuad  6 = BlessingOfHeave
    3 = UnderTheSea   7 = BlessingOfEarth
    4 = UnderTheRiver 8 = BlessingOfMan

    -=-=-=-=-=-=-=-=-=-Result-=-=-=-=-=-=-=-=-=-
    [(🀝,🀝),(🀚,🀛,🀜),Chi(🀚,🀛,🀜),(🀞,🀟,🀠),(🀞,🀟,🀠)]
    2 Han / 30 Fu           2900 pts
    ------      Fu      ------
    Base  20
    ClosedRon  10
    ------     Yaku     ------
    AllSimple  1
    NoPointsHand  1
    -=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-

    -=-=-=-=-=-=-=-=-=-Result-=-=-=-=-=-=-=-=-=-
    [(🀠,🀠),(🀚,🀛,🀜),Chi(🀚,🀛,🀜),(🀝,🀞,🀟),(🀝,🀞,🀟)]
    1 Han / 30 Fu           1500 pts
    ------      Fu      ------
    Base  20
    SingleWait  2
    SingleWait  2
    ------     Yaku     ------
    AllSimple  1
    -=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-

    Enter the hand you want to calculate:
    quit
    Goodbye.
    ```

6. Lastly, you can use ```stack test``` to run tests

    ```bash
    stack test
    ```
