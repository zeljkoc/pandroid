{$DEFINE android22}

uses
   {$IFDEF android27} androidr27 {$else no android27}
   {$IFDEF android25} androidr25 {$else no android25}
   {$IFDEF android22} androidr22 {$else no android22}
   {$IFDEF android19} androidr19 {$else no android19}
   {$IFDEF android16} androidr16 {$else no android16}
   androidr15
   {$ENDIF android16}
   {$ENDIF android19}
   {$ENDIF android22}
   {$ENDIF android25}
   {$ENDIF android27}

