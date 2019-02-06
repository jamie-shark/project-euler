#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

type namedNumber = { number: int; name: string }

let words = [
    { number = 9  ; name = "nine"      }
    { number = 8  ; name = "eight"     }
    { number = 7  ; name = "seven"     }
    { number = 6  ; name = "six"       }
    { number = 5  ; name = "five"      }
    { number = 4  ; name = "four"      }
    { number = 3  ; name = "three"     }
    { number = 2  ; name = "two"       }
    { number = 1  ; name = "one"       }
    { number = 90 ; name = "ninety"    }
    { number = 80 ; name = "eighty"    }
    { number = 70 ; name = "seventy"   }
    { number = 60 ; name = "sixty"     }
    { number = 50 ; name = "fifty"     }
    { number = 40 ; name = "forty"     }
    { number = 30 ; name = "thirty"    }
    { number = 20 ; name = "twenty"    }
    { number = 19 ; name = "nineteen"  }
    { number = 18 ; name = "eighteen"  }
    { number = 17 ; name = "seventeen" }
    { number = 16 ; name = "sixteen"   }
    { number = 15 ; name = "fifteen"   }
    { number = 14 ; name = "fourteen"  }
    { number = 13 ; name = "thirteen"  }
    { number = 12 ; name = "twelve"    }
    { number = 11 ; name = "eleven"    }
    { number = 10 ; name = "ten"       }
]

let asWord n =
    let namedNumber = words |> Seq.find (fun r -> r.number = n)
    namedNumber.name

asWord 1  |> Is "one"
asWord 19 |> Is "nineteen"

