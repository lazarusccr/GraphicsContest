Angle 0
ScaleChange 1.5 0.8 StartFastEndSlow 
SendEvent 1 // jump started from road
Wait 0.15
SendEvent 2  // ascend half height: test collision with second floor tileengine
Wait 0.2
SendEvent 3  // car is in the air: no collision test
Wait 0.45
ScaleChange 1 0.6 StartSlowEndFast
Wait 0.45
SendEvent 4  // descend half height: test collision with second floor tileengine
Wait 0.10
SendEvent 5
Wait 0.05
SendEvent 0