module Test where

    import Encode
    import Decode

    testData = "l5:coordl1:A2:10ee"
    testData2 = "l6:result3:HIT4:prevl5:coordl1:A2:10ee5:coordl1:C1:4ee"
    testLongBoy = "l4:prevl5:coordl1:I1:1e6:result4:MISS4:prevl6:result4:MISS5:coordl1:E1:9e4:prevl4:prevl4:prevl6:result4:MISS4:prevl6:result3:HIT5:coordl1:G1:5e4:prevl4:prevl6:result3:HIT5:coordl1:J1:1e4:prevl4:prevl5:coordl1:A1:2e4:prevl4:prevl6:result4:MISS4:prevl5:coordl1:E1:8e4:prevl6:result4:MISS5:coordl1:H1:9e4:prevl6:result4:MISS5:coordl1:J1:5e4:prevl4:prevl6:result3:HIT5:coordl1:C1:8e4:prevl6:result3:HIT4:prevl6:result4:MISS4:prevl5:coordl1:A1:9e4:prevl6:result4:MISS4:prevl5:coordl1:I1:3e6:result3:HIT4:prevl6:result4:MISS5:coordl1:J1:2e4:prevl5:coordl1:C1:8eeee5:coordl1:C1:2ee6:result4:MISSe5:coordl1:J1:4ee5:coordl1:B1:4eee6:result4:MISS5:coordl1:F1:5eeee6:result4:MISSe5:coordl1:D1:6ee6:result4:MISS5:coordl1:C1:5ee6:result4:MISSe6:result4:MISS5:coordl1:J1:7eee6:result3:HIT5:coordl1:B1:1eee5:coordl1:G1:3ee5:coordl1:D2:10e6:result3:HITe5:coordl1:G2:10e6:result4:MISSeee6:result4:MISS5:coordl1:F1:3ee"

    test :: String -> String;
    test ok = 
        case parseStringToMessage testLongBoy of
            Left parseErr -> "Hmmm"
            Right gameMessage -> convert gameMessage