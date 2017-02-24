(p increment-resoning
 =goal>
   isa goal
   state increment-reasoning
 =retrieval>
  isa reasoning
  level =level
 =imaginal>
  state ???
  buffer empty
==>
 !bind! =levelup (+=level 1)
 =retrieval>	
  level =levelup
 =goal>
  state stop
 -retieval>
 )