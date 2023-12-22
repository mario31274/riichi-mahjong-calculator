# Revision history for riichi-mahjong

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## 0.1.0.1 -- 2023-12-21

* Bug Fixed: Terminal or Wind tiles being grouped as Single meld in
  non Thirteen Orphans hands.

  The hand was matched as [(🀛,🀛),(🀀,🀀),(🀁,🀁),(🀂,🀂),(🀃,🀃),🀀,🀁,🀂,🀃]
  because of the ThirteenOrphans pattern, with the E S W N winds
  being matched as Single meld. This bug only happens when the
  remaining tiles are all terminal or honor tiles. Added another check
  function that prevents any list of melds other than five, seven, or
  13 melds long being treated as a valid winning hand.

## 0.2.0.0 -- 2023-12-21 11:03 pm

* Calculator with prompt completed.
