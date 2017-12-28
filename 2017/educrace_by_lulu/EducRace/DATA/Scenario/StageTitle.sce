MoveXRelative -2000 0 Linear
Skew -80 0 0 Linear
Wait 2
MoveXRelative 2000 1  Sinusoid
Wait 1
Skew 80 0 0.5 StartFastEndSlow
Wait 0.5
Skew 0 0 0.5 StartSlowEndFast
Wait 2.5
Skew -80 0 0.3 Sinusoid
MoveXRelative 1000 0.8 Sinusoid
Wait 2
Kill