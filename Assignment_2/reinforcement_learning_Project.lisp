;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
;;;;;;	REINFORCEMENT LEARNING ACT-R model in order to show the developmental transitions in reasoning about false beliefs of others. 
;;;;;;  These stand from a child's reasoning from his/her own point of view (zero-order) to taking into consideration 
;;;;;;  an other agent’s beliefs (first-order) and later to taking into consideration an other agent’s beliefs about 
;;;;;;  other agents’ beliefs (second-order).
;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
;;;;;;	Instructions on using this model:
;;;;;;		1. Make sure you have ACT-R 6.0
;;;;;;		2. Call (fbt) if you run the model for 1 child doing the experiment 1 time
;;;;;;    3. Call (do-fbt) if you run the model for 1 child doing the experiment 100 times
;;;;;;    4. Call (do-fbt-n N) if you run the model for N children doing the experiment 100 times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar *response* nil) ;; global variable that stores model output
(defvar *model* nil)
(defvar *hold-responses* nil)  ;; global variable that stores model output



;; do experiment 1 time    
(defun fbt (&optional who)

(goal-focus goal)
  (if (eq who 'human)
      (setf *model* nil)
         (setf *model* t)
   )
        (run 30 :real-time t) 
)


;; do experiment 100 times 
(defun do-fbt ()  	
	(dotimes (i 100)
            (setf *response* nil)					 		
			(fbt)
               (push *response* *hold-responses*)
         )                       
)

;; do experiment nx100 times 
(defun do-fbt-n (n)    
	  (dotimes (i n)	
              (setf *hold-responses* nil)                	
                        (reset)				 		
			(do-fbt)
                        (write-to-file (concatenate 'string "dat-" (write-to-string i)) (nreverse *hold-responses*))
;(setf *hold-response* nil)
         )
)

;; write results to a file
(defun write-to-file (name lst)
	(with-open-file 
		(out 
			(ensure-directories-exist 
				(merge-pathnames 
					(make-pathname :name name :directory '(:relative "ACTR_model_output") :type "txt")  
					"~/")
                        )
			:direction :output :if-does-not-exist :create :if-exists :supersede
                 )
			(dolist (line lst)
				(format out "~{~a~^~t~}~%" line))
         )
)

	

(clear-all) ;; clear model settings

(define-model FBT_2   ;name of the model

;;Model Parameters:

;;Below you see the related parameters for the model. You’re expected to assign values to them or keep them default or turn them on (t) or off (nil)
;; you can change the parameters once you have a complete model at the end of the course.
;; when you write your scientific report, you are expected to explain your decisions about the parameters.

(sgp 

	:esc t;  sub-symbolic level
	:ol  t;  optimised learning
	:rt  -4;  retrieval threshold 
	:ans nil;  instantaneous noise
  :egs 1;  utility noise
  :ul  t;  utility learning
  :pas nil; permanent noise 

;; The parameters below for tracking the model’s trace. You can change them if you need to turn off the details etc.
;; See reference manual of ACT-R for further details, which is in the “docs” folder of ACT-R’s main folder

        :ult t ;turn on the trace for the utilities
        :v  t   ; trace detail
        :act low  ; activation trace parameter
        :sact low ;save activation trace

)

;Chunk-types of the Model:

;You will use the following chunk-type for the story facts. Story facts are the sentences from the story, i.e., see lines 141-146.
;The slot "subject" is for the subjects (i.e., "maxi", "sally", "mother").
;The slot "negation" is for negating the verbs (i.e., either nil or "not").
;The slot "verb" is for the verbs (i.e., "put", "see").
;The slot "object" is for the object (i.e., "chips").
;The slot "location" is for the locations (i.e., "cupboard", "oven", "trashbin").
;The slot "time" is for when the story is happened (i.e., 1,2,3, which represent time t0, t1, t2)(see below the temporal order chunk explanation, lines 141-147).
;The slot "type" is holding the type of a verb (i.e., action, perception).
;The slot "self-ref" refers to the chunks name itself. Each story fact chunk will refer to itself.
;The slot "ref" refers to other story facts which occur at the same time. (see below the temporal order chunk explanation, i.e., lines 141-146).
;The slot "level" is to hold the previously retrieved level of reasoning chunk in order to use it when it is necessary. (You might need this at some point in first-order and second-order reasoning)

(chunk-type story subject negation verb object location time type self-ref ref)


;For the time sequences of the events. (You might need these but it is not necessary if you find other ways to model).

(chunk-type tijd t0 t1 t2 t3 t4)     

; The following chunk type is for the goals and their states. 
; The slot "type" is for the where question (action)
; The slot "output"  is for holding the output if the model (i.e., cupboard, oven, trashbin)
; strategy is used to know what strategy we are applying. This has to do with reusing code
(chunk-type goal state type output strategy) 


(add-dm

;The story fact chunks
(not isa chunk) (action isa chunk) (perception isa chunk) (subject isa chunk) 
(location isa chunk) (verb isa chunk) (object isa chunk) (negation isa chunk)
(put isa chunk) (see isa chunk) (type isa chunk) (maxi isa chunk) (sally isa chunk) (chips isa chunk) (mother isa chunk)

;goal state chunks. You’re expected to write the goal state chunks below 
(retrieve isa chunk) (answerZero isa chunk) (finish isa chunk) (continueOrAnswerFirst isa chunk) (chooseStrategy isa chunk)
(maxiPerception isa chunk) (seen isa chunk) (findAction isa chunk) (retrievemax isa chunk) (answersecond isa chunk)
(seensally isa chunk) (answerfirst isa chunk) (findactionmaxi isa chunk)

;temporal order chunk. There are three seperate time points in the story:
; at t0 Maxi put the chips into the cupboard.
; at t1 Sally put the chips into the oven.
; at t1 Maxi saw Sally.
; at t1 Sally did not see Maxi.
; at t2 The mother put the chips into the trashbin.  
 (t0 ISA tijd t0 1 t1 2 t2 3)


;Here, you are expected to write the model's knowledge representations about the story facts (i.e., lines 140-146) based on the defined story chunk-type above.

(firstSentence isa story subject maxi negation nil verb put object chips location "cupboard" time 1 type action self-ref firstSentence ref nil)
(secondSentence isa story subject sally negation nil verb put object chips location "oven" time 2 type action self-ref secondSentence ref nil)
(thirdSentence isa story subject maxi negation nil verb see object sally location nil time 2 type perception self-ref thirdSentence ref secondSentence)
(fourthSentence isa story subject sally negation not verb see object maxi location nil time 2 type perception self-ref fourthSentence ref thirdSentence)
(fifthSentence isa story subject mother negation nil verb put object chips location "trashbin" time 3 type action self-ref fifthSentence ref nil)



;Below chunk assigns the goal at the beginning of the trial
 (goal isa goal state get type action)
)

; Asks retrieval buffer if a story exists with type action and time 3 (the latest location of the bag chips)
(p match
  =goal>
    isa  goal
    state get
    type =act
  ==>
  =goal>
    state  retrieve
  -imaginal>
  +retrieval>
    isa    story
    type   =act
    time   3
)

; If a story is retrieved, set goal state to chooseStrategy and store the story in the imaginal buffer. 
; After this it either goes into answerZero or beginFirstOrder
(p retrieved
  =goal>
    isa goal
    state retrieve
  =retrieval>
    isa story
    subject  =sub
    negation nil
    object   =obj
    location =loc
    time     =tim
    type     =typ
  ?imaginal>
   state     free
   buffer    empty
  ==>
  =goal>
    state    chooseStrategy
  +imaginal>
    isa story
    subject  =sub
    negation nil
    object   =obj
    location =loc
    time     =tim
    type     =typ
)

; If chooseStrategy is the state of the goal and the activation of zeroResponse is higher than beginFirstResponse, give and print the zero order response
(p zeroResponse
   =goal>
    isa    goal
    state  chooseStrategy
   =imaginal>
    isa    story
    location =loc
  ==>
    =goal>
      state  finish
      output =loc
    !output! (=loc)
    !safe-eval! (push 0 *response*)
;    !safe-eval! (push (spp (zeroResponse beginFirstResponse) :name :utility :u :at :reward) *response*)
    !safe-eval! (push (spp (zeroResponse beginFirstResponse beginSecondResponse) :name :utility :u :at :reward) *response*)      
)

; Choose the second order response strategy
(p beginSecondResponse
   =goal>
    isa   goal
    state chooseStrategy
  =imaginal>
    isa    story
==>
  =goal>
    state     retrievemax
    strategy  2
  =imaginal>
)

; Choose the first order response strategy
(p beginFirstResponse
  =goal>
    isa   goal
    state chooseStrategy
  =imaginal>
    isa    story
==>
  =goal>
    state     retrievemax
    strategy  1
  =imaginal>
)

; It will retrieve if Maxi has seen the action (which was stored in the imaginal buffer)
(p retrieveMaxiSeen
   =goal>
    isa      goal
    state    retrievemax
   =imaginal>
    isa      story
    time     =t
    subject  =sub
  ==>
   =goal>
    state seen
   +retrieval>
     isa      story
     time     =t
     negation nil
     subject  maxi ;check if Maxi saw the action. This is so we can have first-order reasoning about what Maxi thinks.
     type   perception
     object   =sub
)

; If goal state is seen and the retrieval of the perception has succeeded, the answer is first order
(p isSeen
  =goal>
   isa   goal
   state seen
  =retrieval>
   isa   story
==>
  =goal>
   state answerFirst
)

; If goal state is seen and if the retrieval has returned nothing, search for a perception of Maxi without a negation
(p notSeen
  =goal>
   isa   goal
   state seen
  ?retrieval>
   buffer empty ;check if retrieval failed
==>
  =goal>
   state    maxiPerception
  -imaginal>
  +retrieval>
   isa      story
   subject  maxi
   negation nil
   type     perception
)

; If goal state is maxiPerception and maxi has seen something else, and search for an action at that time
(p hasMaxiSeen
  =goal>
   isa   goal
   state maxiPerception
  =retrieval>
   isa    story 
   time   =t
   object =obj
==>
  =goal>
   state   findAction
  -imaginal>
  +retrieval>
   isa     story
   subject =obj
   object  chips
   time    =t
   type    action
)

; If goal state is findAction and if an action is found, then put retrieved story in imaginal and give first order reply
(p actionFound
  =goal>
   isa   goal
   state findAction
  =retrieval>
    isa story
    subject  =sub
    negation nil
    object   =obj
    location =loc
    time     =tim
    type     =typ
  ?imaginal>
   state   free
   buffer  empty
==>
  =goal>   
    state  continueOrAnswerFirst
  +imaginal>
    isa story
    subject  =sub
    negation nil
    object   =obj
    location =loc
    time     =tim
    type     =typ
)

; if goal state is continueOrAnswerFirst and strategy is 1, give first order response
(p firstResponse
  =goal>
    isa      goal
    state    continueOrAnswerFirst
    strategy 1
   =imaginal>
    isa    story
    location =loc
  ==>
    =goal>
      state finish
    !output! (=loc)
    !safe-eval! (push 1 *response*)
   ; !safe-eval! (push (spp (zeroResponse beginFirstResponse) :name :utility :u :at :reward) *response*)
    !safe-eval! (push (spp (zeroResponse beginFirstResponse beginSecondResponse) :name :utility :u :at :reward) *response*)    
)

; If state is continueOrAnswerFirst, ask if sally knows if maxi has seen the action in imaginal
(p hasSallySeenMaxi
  =goal>
   isa      goal
   state    continueOrAnswerFirst
   strategy 2
  =imaginal>
   isa     story
   subject =sub
==>
  =goal>
    state  seenSally
  +retrieval>
    isa     story
    subject =sub
    object  maxi
    type    perception
)

(p notSeenSally
  =goal>
   isa   goal
   state seenSally
  ?retrieval>
   buffer empty
==>
  -imaginal>
  =goal>
   state   findActionMaxi
  +retrieval>
   isa     story
   subject maxi
   type    action
)

(p maxiActionFound
  =goal>
   isa   goal
   state findActionMaxi
  =retrieval>
   isa   story
   subject  =sub
   object   =obj
   location =loc
   time     =tim
   type     =typ
  ?imaginal>
   state   free
   buffer  empty
==>
  =goal>
   state  answerSecond
  +imaginal>
   isa story
   subject  =sub
   object   =obj
   location =loc
   time     =tim
   type     =typ
)

; if goal state is answerSecond, give second order response
(p secondResponse
  =goal>
    isa      goal
    state    answerSecond
    strategy 2
   =imaginal>
    isa    story
    location =loc
  ==>
    =goal>
      state finish
    !output! (=loc)
    !safe-eval! (push 2 *response*)
    !safe-eval! (push (spp (zeroResponse beginFirstResponse beginSecondResponse) :name :utility :u :at :reward) *response*)
)


; Utilities of zeroResponse, beginFirstResponse and beginSecondResponse
(spp zeroResponse :u 40)
(spp beginFirstResponse :u 20)
(spp beginSecondResponse :u 10)

; Rewards of zeroResponse, beginFirstResponse and beginSecondResponse
(spp zeroResponse :reward 0)
(spp beginFirstResponse :reward 0)
(spp beginSecondResponse :reward 1)


)

                                                            