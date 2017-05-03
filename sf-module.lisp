;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2009 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filename    : sf-module.lisp
;;; Version     : 5.0
;;;
;;; Description : Module and device for connecting to the hacked synchronous python SF
;;;               game via a socket connection along with the code to run the model
;;;               and game in that way.
;;;
;;; Bugs        :
;;;
;;; To do       :
;;;
;;; ----- History -----
;;;
;;; 2009.03.26 Dan
;;;             : * Initial version with "nothing" in it...
;;; 2009.12.01 Dan [1.1]
;;;             : * Stable version that can only handle the orbit condition
;;;             :   for now since all it sees is the ship itself.
;;; 2009.12.04 Dan
;;;             : * Changing the slack hook to just schedule update-world
;;;             :   everytime since that's safe to do now and that will cause
;;;             :   conflict-resolution to "move back" when necessary.
;;; 2010.01.25 Dan
;;;             : * Added the code to load the other modules used at the end
;;;             :   instead of assuming that they're in place when ACT-R itself
;;;             :   is loaded -- which means the "speculative" modules need to
;;;             :   be in the right place.
;;; 2010.01.28 Dan
;;;             : * Added a wait to the connection code to make sure we get the
;;;             :   initial time before starting and also adding a game update
;;;             :   right at the start of the run now.
;;;             : * In addition, changed the time at which the slack schedules
;;;             :   the update because otherwise we're behind a tick in perception
;;;             :   because of how the game handles things (our 'ready' signal
;;;             :   is an action it processes before sending us the data which
;;;             :   will be followed by the next stopping time).
;;; 2010.02.04 Dan
;;;             : * Added the fortress, explosion, bonus, and vlner objects to
;;;             :   be seen by the model and changed it so that some items are
;;;             :   now "dynamic" meaning they may or may not be visible in
;;;             :   every update.  The assumption is that an item is not visible
;;;             :   if it is dynamic unless the update has an instance for it.
;;; 2010.02.11 Dan
;;;             : * When the fortress explodes make that visible to the model --
;;;             :   assume if there was a fortress but isn't now then a
;;;             :   fortress-explosion is to be shown.
;;;             :
;;; 2010.02.15 Dan
;;;             : * Tweak the kind of the bonus item so that the visual-search
;;;             :   buffer can detect when it's new since the finsts (attended)
;;;             :   info isn't available in the chunks to test.
;;; 2010.02.22 Dan
;;;             : * Make the update-world event non-maintenance because if the
;;;             :   model isn't actively tracking (moving attention to the bonus
;;;             :   item for example) then there are no model events to bring
;;;             :   conflict-resolution "back" to and it can still get stuck
;;;             :   with a long delay from temporal or declarative.
;;; 2010.02.23 Dan
;;;             : * Added the 2-handed and attend-and-track loaders to the end.
;;; 2010.03.01 Dan
;;;             : * Changed get-internal-real-time to (win::gettickcount) in
;;;             :   the logging of the socket data for the higher resolution
;;;             :   that it provides.  This does make it ACL specific for now.
;;; 2010.03.04 Dan
;;;             : * Modified show-ship-trial to display the mine as well when
;;;             :   it exists.
;;; 2010.03.11 Dan
;;;             : * Fixed some issues with a fortress explosion being "visible"
;;;             :   at the start of a trial if it was available on the previous
;;;             :   one and the ship's explosion continuing to be seen during
;;;             :   the 1 second of down-time.
;;; 2010.03.15 Dan
;;;             : * Adding a slot to the ship to indicate when it hits the
;;;             :   fortress hex.  That will stay set until there is a change
;;;             :   to the ships velocity vector.
;;; 2010.03.19 Dan
;;;             : * Fixed the fortress location info -- it's at 355,315.
;;;             : * Added code to check whether or not a ship's shot will hit
;;;             :   the mine and to do the prediction to see if it will ever
;;;             :   be in a position to shoot it given it's current drift and
;;;             :   orientation or if it will need to turn.
;;; 2010.03.26 Dan
;;;             : * Scheduling the proc-display calls in the vis-loc-to-obj
;;;             :   method because otherwise bad things occur with respect to
;;;             :   chunk deletion...
;;; 2010.03.31 Dan
;;;             : * Modifying the mine side to be ahead/behind instead of
;;;             :   left/right/ahead so the model can be more sensitive
;;;             :   to ones coming from behind.  The prediction code now also
;;;             :   computes a turn to both sides and stops at the closer one.
;;;             : * Now also determines when it needs to start dealing with
;;;             :   the mine based using the distance as the key since the
;;;             :   temporal is already in use.  The prediction has a slot for
;;;             :   the mine distance at which the model needs to switch focus
;;;             :   to the mine based on the "soonest" it can shoot it if the
;;;             :   ship doesn't change velocity.
;;; 2010.04.01 Dan
;;;             : * Decreasing the preference to drift from the mine prediction
;;;             :   code.  Now the drift only is if it's 9 or fewer ticks and
;;;             :   the drift instead is if it's a dif of 5.
;;; 2010.04.27 Dan
;;;             : * Fixed an issue with shooting at the mine where the prediction
;;;             :   could have it turn the wrong way when it comes in at about 90
;;;             :   degrees to the ship orientation.
;;; 2010.05.05 Dan
;;;             : * Modified update-world so that the fortress-hit is only
;;;             :   visible when there is still a fortress around.
;;; 2010.05.13 Dan
;;;             : * To improve running performance added the redefinition of
;;;             :   the ACT-R model structure to tweak the hash-table settings.
;;; 2010.06.15 Dan
;;;             : * Adjusted the mine prediction code to increase the action
;;;             :   time for imaginal to be 300ms so, now assume 12 ticks before
;;;             :   any turning.
;;; 2010.06.17 Dan
;;;             : * Adjusted the mine prediction code to increase the action
;;;             :   time for imaginal to be 250ms so, now assume 10 ticks before
;;;             :   any turning.
;;; 2010.06.23 Dan
;;;             : * Reverted to a 200ms action for the prediction and added
;;;             :   a new imaginal action for returning to orbit after shooting
;;;             :   a mine.
;;; 2010.09.10 Dan
;;;             : * Added a speed-change slot to the ship and changed the
;;;             :   "reassess" distance (the hit-dist) from hit -25 to hit - 50
;;;             :   so the model waits a little longer before trying to fix
;;;             :   things.
;;; 2010.09.17 Dan
;;;             : * Changed the reassess from -50 to -90 to try waiting even
;;;             :   a little more...
;;; 2010.11.24 Dan
;;;             : * Cleaned up a few things for distribution.  Basically, removed
;;;             :   the ACL specific code for recording events and drawing debuging
;;;             :   info eventhough nobody would have been using that anyway.
;;; 2011.08.17 Dan
;;;             : * Updated to work with ACT-R r1113.  Required changing the
;;;             :   slack function to accept two parameters.
;;; 2011.09.15 Dan [2.0]
;;;             : * Start of the rewrite to work with the new interface Shawn
;;;             :   created instead of the old one Ben and I put together.
;;; 2011.09.20 Dan
;;;             : * Only pushing data onto the incoming record from both
;;;             :   places for now i.e. both incoming and outgoing are on the
;;;             :   incoming list.  It also has the model time as the first
;;;             :   element.
;;; 2011.09.26 Dan
;;;             : * Adding some hook functions for data recording purposes.
;;;             :   :mid-fixation gets called at the middle time of the
;;;             :    fixation screens with the last mid-fixation time and the
;;;             :    current mid-fixation time as the parameters.
;;;             :   :game-start gets called with the current time when the
;;;             :    first game event occurs.
;;;             :   :game-end gets called with the last game start time and the
;;;             :    current time when the score display occurs.
;;; 2011.09.28 Dan
;;;             : * Updated to use the additions Shawn has added to fill in the
;;;             :   ship details and add the collisions info.
;;; 2015.08.13 Dan [3.0]
;;;             : * Updating to work with ACT-R 7.
;;;             : * Eliminated the sf-object chunk-type since it wasn't really
;;;             :   used for anything and make bonus and trial-end chunk-types
;;;             :   unique by adding a default slot matching the type name with
;;;             :   a value of t.
;;; 2015.08.20 Dan
;;;             : * Removed a call to update-world in collect-sf-incoming-data
;;;             :   because it seemed unnecessary and could cause problems
;;;             :   since it would be called from the "other" thread.
;;;             : * Removed an unnecessary test from the :quit situation so that
;;;             :   it always stops the model when the task is over.
;;; 2015.08.24 Dan
;;;             : * Start of reworking the connection logic to make sure things
;;;             :   are repeatable because there appears to be some discrepancies
;;;             :   with when an event occurs in the game even with a deterministic
;;;             :   model and game.  I think it's because of how the slack hook,
;;;             :   background data collection process, and key handler methods
;;;             :   interact, but can't really pin it down.  Here's an example
;;;             :   from two traces of the same game and model that shows the
;;;             :   issue (the model time is 20s ahead of the game time because
;;;             :   of the info screens).  The model actions should be associated
;;;             :   with the next game update time (the update-world calls).
;;;             :   The keypress at time 35.918 should happen at 35.939 since
;;;             :   that is the next game update time, but in the second log it
;;;             :   isn't registered until two updates later at 36.005.
#|

Model trace (same for both games):

    35.741   SF                     UPDATE-WORLD
    35.741   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.741   PROCEDURAL             CONFLICT-RESOLUTION
    35.743   MOTOR                  PREPARATION-COMPLETE style DELAYED-PUNCH hand RIGHT
    35.743   PROCEDURAL             CONFLICT-RESOLUTION
    35.763   MOTOR                  INITIATION-COMPLETE style DELAYED-PUNCH hand RIGHT
    35.763   PROCEDURAL             CONFLICT-RESOLUTION
    35.767   TEMPORAL               Incrementing time ticks to 21
    35.767   TEMPORAL               MOD-BUFFER-CHUNK TEMPORAL
    35.767   PROCEDURAL             CONFLICT-RESOLUTION
    35.773   MOTOR                  OUTPUT-KEY #(7 4)
    35.773   PROCEDURAL             CONFLICT-RESOLUTION
    35.774   SF                     UPDATE-WORLD
    35.774   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.774   PROCEDURAL             CONFLICT-RESOLUTION
    35.807   SF                     UPDATE-WORLD
    35.807   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.807   PROCEDURAL             CONFLICT-RESOLUTION
    35.807   PROCEDURAL             PRODUCTION-SELECTED THRUST-FAST-LONG
    35.807   PROCEDURAL             BUFFER-READ-ACTION GOAL
    35.807   PROCEDURAL             BUFFER-READ-ACTION VISUAL
    35.807   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    35.807   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL-LEFT
    35.837   MOTOR                  RELEASE-KEY #(7 4)
    35.840   SF                     UPDATE-WORLD
    35.840   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.846   TEMPORAL               Incrementing time ticks to 22
    35.846   TEMPORAL               MOD-BUFFER-CHUNK TEMPORAL
    35.857   PROCEDURAL             PRODUCTION-FIRED THRUST-FAST-LONG
    35.857   PROCEDURAL             MODULE-MOD-REQUEST GOAL
    35.857   PROCEDURAL             MODULE-REQUEST MANUAL
    35.857   GOAL                   MOD-BUFFER-CHUNK GOAL
    35.857   PROCEDURAL             CLEAR-BUFFER MANUAL
    35.857   MOTOR                  DELAYED-PUNCH HAND LEFT FINGER MIDDLE DELAY 0.16
    35.857   PROCEDURAL             CONFLICT-RESOLUTION
    35.873   SF                     UPDATE-WORLD
    35.873   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.873   PROCEDURAL             CONFLICT-RESOLUTION
    35.888   MOTOR                  PREPARATION-COMPLETE style DELAYED-PUNCH hand LEFT
    35.888   PROCEDURAL             CONFLICT-RESOLUTION
    35.906   SF                     UPDATE-WORLD
    35.906   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.906   PROCEDURAL             CONFLICT-RESOLUTION
    35.908   MOTOR                  INITIATION-COMPLETE style DELAYED-PUNCH hand LEFT
    35.908   PROCEDURAL             CONFLICT-RESOLUTION
    35.914   MOTOR                  FINISH-MOVEMENT style DELAYED-PUNCH hand RIGHT
    35.914   PROCEDURAL             CONFLICT-RESOLUTION
    35.914   PROCEDURAL             PRODUCTION-SELECTED SHOOT-FAST-SECOND
    35.914   PROCEDURAL             BUFFER-READ-ACTION GOAL
    35.914   PROCEDURAL             BUFFER-READ-ACTION VISUAL
    35.914   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    35.914   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL-LEFT
    35.914   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL-RIGHT
    35.914   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    35.914   PROCEDURAL             BUFFER-SEARCH VISUAL-SEARCH
    35.918   MOTOR                  OUTPUT-KEY #(3 4)
    35.932   TEMPORAL               Incrementing time ticks to 23
    35.932   TEMPORAL               MOD-BUFFER-CHUNK TEMPORAL
    35.939   SF                     UPDATE-WORLD
    35.939   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.964   PROCEDURAL             PRODUCTION-FIRED SHOOT-FAST-SECOND
    35.964   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
    35.964   PROCEDURAL             MODULE-MOD-REQUEST GOAL
    35.964   PROCEDURAL             MODULE-REQUEST MANUAL
    35.964   GOAL                   MOD-BUFFER-CHUNK GOAL
    35.964   PROCEDURAL             CLEAR-BUFFER VISUAL-SEARCH
    35.964   PROCEDURAL             CLEAR-BUFFER MANUAL
    35.964   MOTOR                  DELAYED-PUNCH HAND RIGHT FINGER INDEX DELAY 0.09
    35.964   PROCEDURAL             CONFLICT-RESOLUTION
    35.972   SF                     UPDATE-WORLD
    35.972   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.972   PROCEDURAL             CONFLICT-RESOLUTION
    35.999   MOTOR                  PREPARATION-COMPLETE style DELAYED-PUNCH hand RIGHT
    35.999   PROCEDURAL             CONFLICT-RESOLUTION
    36.005   SF                     UPDATE-WORLD


Game 1 log:

15774 1440184862.784000 hold-fire,missile-fired
15807 1440184862.789000
15840 1440184862.792000 release-fire
15873 1440184862.796000
15906 1440184862.800000 hit-fortress,vlner-increased
15939 1440184862.804000 hold-thrust
15972 1440184862.809000
16005 1440184862.813000

game 2 log:

15774 1440187421.873000 hold-fire,missile-fired
15807 1440187421.878000
15840 1440187421.881000 release-fire
15873 1440187421.885000
15906 1440187421.889000 hit-fortress,vlner-increased
15939 1440187421.892000
15972 1440187421.896000
16005 1440187421.901000 hold-thrust
|#
;;; 2015.08.27 Dan
;;;             : * Changed the key handlers to use push-last because they need
;;;             :   to be sent in the order they're generated in the event that
;;;             :   a down and up happen in the same time tick.
;;; 2015.09.01 Dan
;;;             : * Adding :sf-key-down-hook and :sf-key-up-hook which will be
;;;             :   passed the world and the key at the "next" update i.e. after
;;;             :   the proc-display has actually happened so that the effects
;;;             :   will have been registered in the percepts and game info.
;;; 2015.09.25 Dan
;;;             : * New screen-types: instructions - requires keypress
;;;             :                     basic-task - doesn't require keypress?
;;; 2015.09.28 Dan
;;;             : * Fix some of the end game transition code for the new screens
;;;             :   so the model starts new games correctly.
;;; 2015.09.30 Dan
;;;             : * The key hook function(s) for a key will continue to be called
;;;             :   until a true value is returned from the hook call for that key.
;;; 2015.10.01 Dan
;;;             : * Adjust for most recent game version which now has the end
;;;             :   score screen as "score" instead of "total_score".
;;; 2015.10.06 Dan
;;;             : * Load the analyze-position file as well from the space-fortress-modules
;;;             :   directory.
;;; 2015.10.07 Dan
;;;             : * Add a few things to the reset function that weren't there
;;;             :   previously and affected repeatability...
;;; 2015.10.19 Dan
;;;             : * Track ship deaths and fortress kills per game.
;;; 2016.07.07 Dan [3.1]
;;;             : * Added a hook function to the module :sf-data-hook which
;;;             :   if set to a function will pass the data line read in to that
;;;             :   function.
;;;             : * Set the value slot of the explosion feature to indicate why
;;;             :   it occurred: inner (hit inner hex), outer (hit outer hex),
;;;             :   or shell (shell hit ship) and set the explosion object
;;;             :   chunk's status slot accordingly (leaving the value as explosion
;;;             :   so there's a general test and to be compatible with older models).
;;;             :   If none of those are true then the value/status will just be
;;;             :   explosion. Still changing the explosion-loc to kind debris
;;;             :   when it's attended.
;;; 2016.07.08 Dan [3.2]
;;;             : * Cheat on the location of an explosion so that the vision
;;;             :   module should always re-encode that as the feature when
;;;             :   tracking the ship.
;;;             : * Also override the default value of :tracking-clear so that
;;;             :   the model should always see the explosions when tracking the
;;;             :   ship.
;;; 2016.07.11 Dan [3.3]
;;;             : * Add features for a single shell and single missile.
;;;             :   If there are any shells then the nearest one to the ship
;;;             :   can be found with a kind shell and if there are any missiles
;;;             :   the nearest one to the fortress is found with kind missile.
;;;             :   Only representing a single at this point to avoid issues
;;;             :   with excessive chunk creation and needing to map multiple
;;;             :   items to the "same" feature to allow tracking -- thus trying
;;;             :   to track either of these only really works as long as there's
;;;             :   only one available.
;;;             : * Add a case for the config screen type which just does nothing
;;;             :   to avoid a warning for the unexpected type.
;;;             : * Fixed a bug with the explosion-loc going from explosion
;;;             :   to debris and then back to explosion for a single ship death.
;;;             : * Added the version check.
;;;             : * Added the values to the shell and missile chunks at the start.
;;; 2016.07.13 Dan [3.4]
;;;             : * Use the total-score notice with a delay of 0 to reset the
;;;             :   the last-time value to 0 since there isn't a basic-game or
;;;             :   fmri-game notice now.
;;;             : * The score screen also needs to reset the timer and add a
;;;             :   dummy delay of 1 second for "mode 2" servers.
;;;             : * But only if the screen type is delay...
;;; 2016.07.14 Dan [3.5]
;;;             : * Set the value of the missile and shell locations to the count
;;;             :   of the corresponding item visible, and the count slot of the
;;;             :   objects.
;;;             :  [4.0]
;;;             : * Turns out for the newest "mode 2" servers there is no type
;;;             :   delay on the score updates.  So, need to add the 1 second
;;;             :   delay to only the events type.  Since this only works with
;;;             :   the newer servers updating the version significantly.
;;;             : * Remove an unnecessary proc-display in mode 1 servers to
;;;             :   avoid a warning about multiple proc-display messages.
;;; 2016.07.22 Dan
;;;             : * Have vis-loc-to-obj only schedule one proc-display to avoid
;;;             :   some warnings.
;;;             : * Only schedule a proc-display if there isn't already one
;;;             :   either waiting because of a device lock or already scheduled
;;;             :   at the same time by the module (replacing last-proc-handled
;;;             :   with last-proc-scheduled to deal with that since the handled
;;;             :   isn't being used for the current synchronous game interface).
;;;             : * Updated the chunk and chunk-type definitions to provide a
;;;             :   label slot in all of the objects to indicate their "type".
;;; 2016.07.25 Dan
;;;             : * Added a pnts-loc and pnts chunk with label points for the
;;;             :   score.  The x,y are 245,700 which probably isn't right, but
;;;             :   puts it on the other side of screen center opposite vlner text.
;;; 2016.09.06 Dan
;;;             : * Updated the written-for-act-r-version to 7.1 since that's the
;;;             :   current one in the repository and avoids a warning since the
;;;             :   major version has changed.
;;;             : * Added support for the staircase game by pulling the bighex
;;;             :   and smallhex values from the input and using those to create
;;;             :   features for the visual chunk.  There are 6 new slots:
;;;             :
;;;             :   - dist-to-bighex & dist-to-smallhex:
;;;             :      distance from current ship point to nearest segment of
;;;             :      the indicated hex measured in pixels
;;;             :
;;;             :   - travel-dist-to-bighex & travel-dist-to-smallhex:
;;;             :      distance from current ship position to the indicated
;;;             :      hex along its current trajectory measured in pixels
;;;             :      or 9999 if it won't intersect the hex
;;;             :
;;;             :   - travel-time-to-bighex & travel-time-to-smallhex:
;;;             :      time in seconds until the ship hits the indicated
;;;             :      hex along its current trajectory measured in seconds
;;;             :      or 9999 if it won't hit
;;; 2016.09.08 Dan [5.0]
;;;             : * Added a play-sf-games which stops things at the score screen
;;;             :   after the indicated number of games have occurred.
;;;             : * Added the ability to set a specific name for the model's data
;;;             :   directory.  If not provided it uses a number written into a
;;;             :   file to name it model_#.  The file is called sf-model-number.txt
;;;             :   and exists in the same directory this file was loaded from.
;;;             : * Cleaned up the communication because there was some skewing
;;;             :   of the updates since the model sent a drawing command before
;;;             :   it read the first config line, and then sent an extra continue
;;;             :   as a result.
;;;             :
;;;             :   == That means the data files generated by a deterministic ==
;;;             :   == model using this update will not be identical to those ==
;;;             :   == using the older version!                               ==
;;;             :
;;; 2016.09.09 Dan
;;;             : * Removed a bunch of old code and object slots that aren't
;;;             :   being used anymore to clean things up.
;;; 2016.09.13 Dan
;;;             : * Added code to work with older version of the game server too
;;;             :   (the refactored one that I have) just to be safe.  The changes
;;;             :   to do that require accepting a nil result for changing the id
;;;             :   as long as the :id value was updated and assuming that the
;;;             :   hex sizes are 200 and 40 if they aren't provided.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; Module and device for connecting ACT-R models to the special python version
;;; of the space fortress task which we have.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "UNI-FILES" "ACT-R:support;uni-files")

;;; Do a version check since it should be run with the updated vision module.

(written-for-act-r-version "7.1" (format nil "Space Fortress module.~%           Requires vision module version 5.1 for updated tracking code"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redefine the ACT-R model structure so that the hash-table is more appropriate
;;; for this task -- reduces the run time for a day from ~6 minutes to just over
;;; 2 with (play-sf (* 210.01 16) :speed 150).
;;;
;;; [3.0] Just set the system parameters now instead of redefining the structure
;;; since that's why they were added.

(ssp :mcts 45000 :mctrt .75)

;;; Variables to hold the connection details.

;; (defparameter *host-address* "127.0.0.1" "Address/name of the machine running the server")
;; (defparameter *port-for-server* 3000 "default port for the server")

;;;;;;;;;;;;;;;;;;;;;
;;; Tools

;;; Communication to the game is via text based commands on one line.  The sending functions are
;;; responsible for proper formatting.

(defconstant *cr/lf* (format nil "~C~C" #\cr #\lf))

(defmacro socket-write-line (world string &rest args)
  "Write a CRLF-terminated line"
  `(if (or (null (socket ,world))
           (uni-stream-closed (socket ,world)))
       (print-warning "Can't send command {~S} because connection closed"
                   (format nil ,string ,@args))
     (let ((string-to-write (format nil ,string ,@args)))
       (when (record-outgoing ,world)
         (push (list :out (mp-time) (get-internal-real-time) string-to-write)
               (data-record ,world)))

       (uni-send-string (socket ,world)
                        (concatenate 'string string-to-write *cr/lf*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The UT device code

;;; Define a class for the device which will also be the instance for the module.

(defclass sf-world ()
  (;; connection info
   (connection :initform nil :accessor sf-connection)
   ;; (host :initform *host-address* :accessor sf-host)
   ;; (port :initform *port-for-server* :accessor sf-port)
   ;; (socket :initform nil :accessor socket)
   (game-state :initform nil :accessor game-state)

   ;; communication logging
   (collect-incoming :initform nil :accessor collect-incoming)
   (record-outgoing :initform nil :accessor record-outgoing)
   (data-record :initform nil :accessor data-record)

   ;; visual feature processing
   (all-locs :initform nil :accessor all-locs)
   (dynamics :initform nil :accessor dynamics)
   (loc->obj-table :initform (make-hash-table :test 'eq) :accessor loc->obj-table)
   (last-proc-scheduled :initform -1 :accessor last-proc-scheduled)

   ;; hacks for visual info missing or otherwise in need of special care
   (fortress-there :initform nil :accessor fortress-there)
   (f-hit-counter :initform nil :accessor f-hit-counter)
   (bonus-timer :initform nil :accessor bonus-timer)
   (hit-hex :initform nil :accessor hit-hex)
   (last-v :initform nil :accessor last-v)
   (mine-visible :initform nil :accessor mine-visible)
   (fort-exists :initform nil :accessor fort-exists)

   ;; actions to send to the game
   (responses :initform nil :accessor responses)
   (auto-response :initform nil :accessor auto-response)

   ;; user hook parameters
   (fix-hook :initform nil :accessor fix-hook)
   (start-hook :initform nil :accessor start-hook)
   (end-hook :initform nil :accessor end-hook)
   (key-down-hook :initform nil :accessor key-down-hook)
   (key-up-hook :initform nil :accessor key-up-hook)
   (key-change-events :initform nil :accessor key-change-events)
   (data-hook :initform nil :accessor data-hook)

   ;; track some game stats as it plays
   (ship-alive :initform nil :accessor ship-alive)
   (game-stats :initform (list (list 0 0)) :accessor game-stats)
   (fortress-killed :initform nil :accessor fortress-killed)

   ;; connection and running control
   (last-time :initform 0 :accessor last-time)
   (name :initform nil :accessor name)
   (game-count :initform 0 :accessor game-count)
   (game-limit :initform -1 :accessor game-limit)
   (change-drawing-mode :initform nil :accessor change-drawing-mode)))


;;; Methods which need to be defined for the device.
;;; All methods should be provided even if not needed
;;; (the cursor and speech methods).

(defmethod device-move-cursor-to ((world sf-world) loc) (declare (ignorable world loc)))
(defmethod device-handle-click ((world sf-world)) (declare (ignorable world)))
(defmethod device-handle-click-release ((world sf-world)) (declare (ignorable world)))

(defmethod get-mouse-coordinates ((world sf-world)) (declare (ignorable world)) (vector 0 0))
(defmethod device-speak-string ((world sf-world) text)(declare (ignorable world text)))

;;; The motor extension module now calls the original keypress
;;; method and adds a keyrelease method as well.

(defmethod device-handle-keypress ((world sf-world) key)
  (push-last (cons 'keydown (model-key-to-sf-key key)) (responses world))
  (push (cons key :down) (key-change-events world)))

(defmethod device-handle-keyrelease ((world sf-world) key)
  (push-last (cons 'keyup (model-key-to-sf-key key)) (responses world))
  (push-last (cons key :up) (key-change-events world)))

;;; To keep things simple it's just assuming the hand positions are over
;;; the correct keys.  The model may set them that way if it wants, but
;;; it will not affect how it plays.
;;;
;;; [2.0] These are now mapped to the keycodes needed instead of the key names.
;;; with the current actions in the config being:  a,w,d for movement (left ring, middle, index respectively)
;;; space for fire (right index) and j for IFF (right middle)
;;; all others are mapped to z as a default.
;;;
;;; [3.0] The model presses are now reported as keys so need to change the
;;; mapping appropriately with the assumption that the models hands will
;;; just be on the home row.  Thus s,d,f are left, thrust, right and
;;; j,k,l are fire, iff, and bonus.


(defun model-key-to-sf-key (key)
  (case key
    (#\d 119)
    (#\s  97)
    (#\f 100)
    (#\j  32)
    (#\k 106)
    (#\l 107)
    (t  122)))


;;; The model's visual world is just maintained as a list of the chunks which are
;;; modified in the message handler.  So, this code is mostly trivial.


(defmethod build-vis-locs-for ((world sf-world) (vis vision-module))
  (mapcan (lambda (x)
            (if (fast-chunk-slot-value-fct x 'visible)
                (list x)
              nil))
    (all-locs world)))


;;; Keep the visual objects in a simple look-up table -- since all the items in
;;; the world are known in advance they can be fixed chunks created at module start
;;; time.  Some features get changed dynamically when attended for convenience (to
;;; differentiate a 'new' feature from one that hasn't been attended since the
;;; finsts are valid for the visual-search buffer) so may need an additional
;;; proc-display to reflect that.

(defmethod vis-loc-to-obj ((world sf-world) vis-loc)
  (let ((need-proc nil))
    (when (eq vis-loc 'bonus-loc)
      (mod-chunk-fct 'bonus-loc '(kind bonus))
      (setf need-proc t))
    (when (eq vis-loc 'trial-end-loc)
      (mod-chunk-fct 'trial-end-loc '(kind trial-end))
      (setf need-proc t))
    (when (and (eq vis-loc 'explosion-loc) (eq (chunk-slot-value explosion-loc kind) 'explosion))
      (mod-chunk-fct 'explosion-loc '(kind debris))
      (setf need-proc t))

    (when need-proc
      (unless (pending-procs (current-device-interface)) ;; don't schedule it if there is already a delayed one for the device interface
        (unless (= (last-proc-scheduled world) (mp-time-ms)) ;; don't schedule it if we've already scheduled one at this time
          (setf (last-proc-scheduled world) (mp-time-ms))
          (schedule-event-now 'proc-display :maintenance t :module :sf))))

    (gethash vis-loc (loc->obj-table world))))


;;; Send the model's actions and any automatic responses that
;;; are generated for some of the between game screens used in
;;; the fMRI task.

(defun send-all-pending-model-actions (world)

  (dolist (x (responses world))
    (sf-connection:connection-send-action (sf-connection world) (car x) (cdr x)))

  (setf (responses world) nil)

  (let (new)

    (dolist (x (auto-response world))
      (sf-connection:connection-send-action (sf-connection world) (car x) (cdr x))
      (when (eq (car x) 'keydown)
        (push-last (cons 'keyup (cdr x)) new)))

    (setf (auto-response world) new))
  (sf-connection:connection-step (sf-connection world)))

;;; Read a line of data from the socket skipping blank lines
;;; and terminating the run if there's an error encountered.

(defun get-line-of-data-from-game (world)
  (handler-case
      (sf-connection:connection-report (sf-connection world))
    (sf-connection:connection-error (c)
      (print-warning "~A. Cannot continue." c)
      (schedule-break-relative 0 :priority :max :details "Game connection error" :time-in-ms t)
      nil)))

;;; Handle all the game updating in an event that gets scheduled to occur based
;;; on the times provided.  This keeps things synchronous and avoids issues with
;;; a separate thread collecting data asynchronously.

(defun update-world (world)
  ;; send everything that's happened since the last update

  (send-all-pending-model-actions world)

  ;; Always send a continue.
  ;; The next update will be scheduled based on the
  ;; time/delay from the game data that's read here.

  ;; (socket-write-line world "continue")

  (let ((data (get-line-of-data-from-game world))
        (new-data nil))

    (unless data (return-from update-world))

    (let* ((bonus-on (fast-chunk-slot-value-fct 'bonus-loc 'visible))
           (delay (if (string-equal (getf data :mode) "delay")
                       (getf data :delay_duration)
                     0)))

      ;; record the data and send it to the hook if needed

      (when (collect-incoming world)
        (push (list :in (mp-time) (get-internal-real-time) data) (data-record world)))

      (awhen (data-hook world) (ignore-errors (funcall it data)))


      ;; mark everything as not visible and set the mine shooting slots to
      ;; indicate non-hitable and very far away because it will be recalculated

      (mapc (lambda (x) (set-chunk-slot-value-fct x 'visible nil)) (dynamics world))

      (set-chunk-slot-value my-ship shoot-at-mine nil)
      (set-chunk-slot-value my-ship hit-fortress-first nil)
      (set-chunk-slot-value my-ship mine-dist 10000)


      ;; Update the features and time info based on the type of info this is

      (case (read-from-string (getf data :screen-type))

        (progress ;; just ignore it
         )

        (basic-task ;; press z to skip it and reset time offset
         (push-last (cons 'keydown 122) (auto-response world))
         (setf (last-time world) 0)
         )

        (instructions ;; press z to skip it
         (push-last (cons 'keydown 122) (auto-response world))
         )

        (wait-caret ;; press the ^ key to skip it
         (push-last (cons 'keydown 94) (auto-response world))
         )

        (fixation ;; assume a 10 second delay if none specified
         (when (zerop delay)
           (setf delay 10000))
         )

        (fmri-task ;; press z to advance and record the visual
                   ;; features for the game type and reset time offset

         (push-last (cons 'keydown 122) (auto-response world))

         (when (zerop delay) ;; assume 10 second delay if none specified
           (setf delay 10000))

         (cond ((getf data :mines)
                (mod-chunk-fct 'cond1-loc (list 'visible t))
                (setf (chunk-real-visual-value 'cond1-loc) "+mine")
                (mod-chunk-fct 'cond1 (list 'value "+mine")))
               (t
                (mod-chunk-fct 'cond1-loc (list 'visible t))
                (setf (chunk-real-visual-value 'cond1-loc) "-mine")
                (mod-chunk-fct 'cond1 (list 'value "-mine"))
                (setf (mine-visible world) nil)))

         (cond ((getf data :fortress)
                (mod-chunk-fct 'cond2-loc (list 'visible t))
                (setf (chunk-real-visual-value 'cond2-loc) "+fort")
                (mod-chunk-fct 'cond2 (list 'value "+fort"))
                (setf (fort-exists world) t))
               (t
                (mod-chunk-fct 'cond2-loc (list 'visible t))
                (setf (chunk-real-visual-value 'cond2-loc) "-fort")
                (mod-chunk-fct 'cond2 (list 'value "-fort"))
                (setf (fort-exists world) nil)))

         (setf (last-time world) 0))


        (foe-mines ;; press z to advance and record the visual
                   ;; features for the mine letters

         (push-last (cons 'keydown 122) (auto-response world))

         (when (zerop delay) ;; assume 10 second delay if none specified
           (setf delay 10000))

         ;; hack to make sure fortress isn't considered blown up
         (setf (fortress-there world) nil)

         (let ((letters (getf data :letters)))

           (mod-chunk-fct 'foe1-loc (list 'visible t))
           (setf (chunk-real-visual-value 'foe1-loc) (string (aref letters 0)))
           (mod-chunk-fct 'foe1 (list 'value (string (aref letters 0))))

           (mod-chunk-fct 'foe2-loc (list 'visible t))
           (setf (chunk-real-visual-value 'foe2-loc) (string (aref letters 1)))
           (mod-chunk-fct 'foe2 (list 'value (string (aref letters 1))))

           (mod-chunk-fct 'foe3-loc (list 'visible t))
           (setf (chunk-real-visual-value 'foe3-loc) (string (aref letters 2)))
           (mod-chunk-fct 'foe3 (list 'value (string (aref letters 2))))))

        (total-score ;; gets sent twice one with a delay and one without
                     ;; in the delay version provide the visual feature
                     ;; so the model sees the end of game and clear some
                     ;; of the hack flags
                     ;; In the no delay version hit z to skip it, record results,
                     ;; and schedule an event for 1 second from not to note the
                     ;; game ended.

         ;; old hack left in
         (setf (mine-visible world) nil)
         (setf (fortress-there world) nil)

         (cond ((zerop delay)
                (push-last (cons 'keydown 122) (auto-response world))
                (push-last (getf data :raw-pnts) (first (game-stats world)))
                (push-last (getf data :bonus) (first (game-stats world)))
                (push-last (getf data :total-bonus) (first (game-stats world)))
                (push-last (append (getf data :base-stats) (getf data :event-stats) (getf data :input-stats)) (first (game-stats world)))

                (push (list 0 0) (game-stats world))
                (schedule-event-relative 1000 (lambda () (incf (game-count world))) :priority :max :time-in-ms t :maintenance t :output nil)
                (setf (last-time world) 0))

               (t
                (mod-chunk-fct 'trial-end-loc (list 'visible t 'kind 'new-trial-end))
                (setf (hit-hex world) nil)
                (setf (last-v world) nil))))


        (score ;; there's only one score message for the current game servers
               ;; when using model_interface 2 which is all this will handle now
               ;; this might cause problems if running with an older space fortress
               ;; game server.
               ;;
               ;; Press z to continue, assume a 1 second delay, show the
               ;; trial-end feature to the model, record the game stats, and
               ;; schedule an event to mark the game as complete after 1 second.

         ;; (push-last (cons 'keydown 122) (auto-response world))

         (mod-chunk-fct 'trial-end-loc (list 'visible t 'kind 'new-trial-end))

         ;; old hacks left in
         (setf (mine-visible world) nil)
         (setf (fortress-there world) nil)
         (setf (hit-hex world) nil)
         (setf (last-v world) nil)

         ;; record the values from this game and set the list for the next game

         (push-last (getf data :raw-pnts) (first (game-stats world)))
         (push-last (getf data :bonus) (first (game-stats world)))
         (push-last (getf data :total-bonus) (first (game-stats world)))
         (push-last (append (getf data :base-stats) (getf data :event-stats) (getf data :input-stats)) (first (game-stats world)))

         (setf (last-time world) 0)
         (setf delay 1000)
         (push (list 0 0) (game-stats world))

         (schedule-event-relative 1000 (lambda () (incf (game-count world))) :priority :max :time-in-ms t :maintenance t :output nil))

        (bonus ;; just quit the game since it got to the end
               ;; and there's nothing else that can be done
               ;; Not used in the current servers (2016), but left in
               ;; anyways.

         (socket-write-line world "quit"))

        (config ;; this is now handled in the connection function
                ;; Shouldn't see any while the game is running.

         (print-warning "Unexpected config data during the task: ~S" data))


        (game ;; This is where all the action happens...

         ;; If the display is being turned on/off between runs
         ;; send that notice now because it's only valid during
         ;; a game update not config or score

         (awhen (change-drawing-mode world)
                (socket-write-line world "drawing ~d" it)
                (setf (change-drawing-mode world) nil))

         ;; compute how long to delay based on the time provided
         ;; unless there's an explicit delay given

         (let ((next-time (getf data :time)))
           (cond ((zerop delay)
                  (setf delay (- next-time (last-time world)))
                  (setf (last-time world) next-time))
                 (t
                  (setf (last-time world) (+ delay (last-time world))))))

         ;; Update all the visual features for the model

         (awhen (getf data :ship)
                (if (getf it :alive)
                    (let ((x (getf it :x))
                          (y (getf it :y))
                          (angle (getf it :angle))
                          (vel (getf it :speed))
                          (dist (getf it :distance-from-fortress))
                          (vdir (getf it :vdir))
                          (orient (getf it :orientation))
                          (vx (getf it :vx))
                          (vy (getf it :vy))
                          (bighex (aif (getf data :bighex) it 200))
                          (smallhex (aif (getf data :smallhex) it 40)))

                      ;; Track velocity info and look for a bounce
                      ;; off of the inner hex (in games where that happens)

                      (cond ((null (last-v world))
                             (setf (last-v world) (cons vx vy)))
                            ((and (= vx (car (last-v world))) (= vy (cdr (last-v world))))
                             ;; do nothing
                             )
                            (t
                             (when (and (numberp (hit-hex world)) (> (hit-hex world) 0))
                               (decf (hit-hex world)))

                             (when (and (numberp (hit-hex world)) (zerop (hit-hex world)))
                               (setf (hit-hex world) nil))

                             (let* ((od (atan (cdr (last-v world)) (car (last-v world))))
                                    (cd (atan vy vx))
                                    (dif (abs (- cd od))))
                               (when (and (> dif 3) (< dif 3.3) (< dist (+ smallhex (* 2 (abs vel))))) ;; close to 180 shift and near inner hex

                                 (setf (hit-hex world) 3)))

                             (setf (car (last-v world)) vx)
                             (setf (cdr (last-v world)) vy)))

                      (let ((last-vel (chunk-slot-value my-ship vel)))
                        (when (null last-vel)
                          (setf last-vel 0))
                        (mod-chunk-fct 'my-ship (list 'x x
                                                      'y y
                                                      'vx vx
                                                      'vy vy
                                                      'angle angle
                                                      'vel vel
                                                      'dist dist
                                                      'vdir (if (= vel 0.0) angle vdir)
                                                      'orientation orient
                                                      'hex-dist nil
                                                      'last-dist (if (numberp (chunk-slot-value-fct 'my-ship 'dist))
                                                                     (chunk-slot-value-fct 'my-ship 'dist)
                                                                   dist)
                                                      'speed-change (cond ((= vel last-vel) 0) ((< vel last-vel) -1) (t 1))
                                                      'hex-hit nil
                                                      'travel-time-to-bighex (travel-time-to-hex vel x y vx vy :radius bighex)
                                                      'travel-time-to-smallhex (travel-time-to-hex vel x y vx vy :radius smallhex)
                                                      'dist-to-bighex (dist-to-hex x y :radius bighex)
                                                      'dist-to-smallhex (dist-to-hex x y :radius smallhex)
                                                      'travel-dist-to-bighex (travel-dist-to-hex vel x y vx vy :radius bighex)
                                                      'travel-dist-to-smallhex (travel-dist-to-hex vel x y vx vy :radius smallhex))))

                      (mod-chunk-fct 'own-ship-loc (list 'screen-x x 'screen-y y 'visible t))

                      (setf (ship-alive world) t)

                      ;; clear the last explosion condition
                      ;; directly because the death collision detection only
                      ;; comes in on one tick and need to know which one
                      ;; so it's easy to just track that in the location chunk
                      ;; instead of in the module itself

                      (mod-chunk explosion-loc value nil))

                  (progn
                    ;; count the death
                    ;; have to be careful now because the game keeps playing
                    ;; and sends multiple ticks while dead

                    (when (ship-alive world)
                      (incf (first (first (game-stats world)))))

                    (setf (ship-alive world) nil)

                    ;; cheat a little and use the last ship position so that
                    ;; re-encoding after tracking should always catch this
                    ;; instead of the real location: (getf it :x) (getf it :y)

                     (let ((x (fast-chunk-slot-value-fct 'my-ship 'x))
                           (y (fast-chunk-slot-value-fct 'my-ship 'y))
                           (collisions (getf data :collisions)))

                       (mod-chunk-fct 'explosion-loc (list 'screen-x x 'screen-y y 'visible t))

                       (unless (fast-chunk-slot-value-fct 'explosion-loc 'value)

                         (mod-chunk explosion-loc kind explosion)

                         (cond ((find :small-hex collisions)
                                (mod-chunk explosion-loc value inner)
                                (mod-chunk explosion status inner))
                               ((find :big-hex collisions)
                                (mod-chunk explosion-loc value outer)
                                (mod-chunk explosion status outer))
                               ((find :shell collisions)
                                (mod-chunk explosion-loc value shell)
                                (mod-chunk explosion status shell))
                               (t
                                (mod-chunk explosion-loc value explosion)
                                (mod-chunk explosion status explosion)))))

                    ;; clear the special flags
                    (setf (hit-hex world) nil)
                    (setf (last-v world) nil))))

         (awhen (getf data :shells)
                (push (list :shells it) new-data)


             (let* (shell-x
                    shell-y
                    (ship-x (fast-chunk-slot-value-fct 'my-ship 'x))
                    (ship-y (fast-chunk-slot-value-fct 'my-ship 'y))
                    best
                    (best-dist 10000))

               (dolist (s it)
                 (let* ((s-x (getf s :x))
                       (s-y (getf s :y))
                       (d (dist (vector ship-x ship-y) (vector s-x s-y))))
                   (when (< d best-dist)
                     (setf best s)
                     (setf best-dist d)
                     (setf shell-x s-x)
                     (setf shell-y s-y))))

               (mod-chunk-fct 'shell-loc (list 'screen-x shell-x 'screen-y shell-y 'visible t 'value (length it)))
               (mod-chunk-fct 'shell (list 'x shell-x
                                           'y shell-y
                                           'vx (getf best :vx)
                                           'vy (getf best :vy)
                                           'orientation (getf best :orientation)
                                           'count (length it)))))

         (awhen (getf data :fortress)
                (when (getf it :alive)

                  (when (fort-exists world) ;; corrects for a bug that I don't want to
                    ;; track down on the python side
                    ;; If the ship is destroyed the fortress
                    ;; indicator gets sent even in the no fortress
                    ;; conditions (mines only since that's the only
                    ;; one where the ship can be destroyed)

                    (mod-chunk-fct 'fortress-loc (list 'visible t))
                    (setf (fortress-killed world) nil)
                    (setf (fortress-there world) t))))

         (awhen (getf data :missiles)

                (let* (missile-x
                       missile-y
                       (fort-x (fast-chunk-slot-value-fct 'fortress-loc 'screen-x))
                       (fort-y (fast-chunk-slot-value-fct 'fortress-loc 'screen-y))
                       best
                       (best-dist 10000))

                  (dolist (m it)
                    (let* ((m-x (getf m :x))
                           (m-y (getf m :y))
                           (d (dist (vector fort-x fort-y) (vector m-x m-y))))
                      (when (< d best-dist)
                        (setf best m)
                        (setf best-dist d)
                        (setf missile-x m-x)
                        (setf missile-y m-y))))

                  (mod-chunk-fct 'missile-loc (list 'screen-x missile-x 'screen-y missile-y 'visible t 'value (length it)))
                  (mod-chunk-fct 'missile (list 'x missile-x
                                                'y missile-y
                                                'vx (getf best :vx)
                                                'vy (getf best :vy)
                                                'orientation (getf best :orientation)
                                                'count (length it)))))

         (awhen (getf data :vlner)
                (mod-chunk-fct 'vlner-loc (list 'visible t))
                (mod-chunk-fct 'vlner (list 'value it))
                (setf (chunk-real-visual-value 'vlner-loc) it))

         (awhen (getf data :pnts)
                (mod-chunk-fct 'pnts-loc (list 'visible t))
                (mod-chunk-fct 'pnts (list 'value it))
                (setf (chunk-real-visual-value 'pnts-loc) it))

         (awhen (getf data :iff)
                (mod-chunk-fct 'iff-loc (list 'visible t))
                (mod-chunk-fct 'iff (list 'value it))
                (setf (chunk-real-visual-value 'iff-loc) it))

         (awhen (getf data :bonus)
                (mod-chunk-fct 'bonus-loc (list 'visible t))

                (if bonus-on
                    (when (> (- (mp-time-ms) (bonus-timer world)) 500)
                      (mod-chunk-fct 'bonus-loc '(kind bonus)))
                  (progn
                    (setf (bonus-timer world) (mp-time-ms))
                    (mod-chunk-fct 'bonus-loc '(kind new-bonus))))

                (mod-chunk-fct 'bonus (list 'value it)))


         (awhen (getf data :collisions)
                (when (find :fortress it)

                  (setf (f-hit-counter world) 0))

                (when (find :small-hex it)
                  (setf (hit-hex world) 3)))


         (awhen (getf data :mine)

                (mod-chunk-fct 'mine-loc (list 'visible t 'kind 'mine))

                ;; record the data for shooting a mine

                (let* ((x (getf it :x))
                       (y (getf it :y))
                       (o (chunk-slot-value my-ship orientation))
                       (sx (chunk-slot-value my-ship x))
                       (sy (chunk-slot-value my-ship y))
                       (ma (rad->deg (atan (- sy y) (- x sx))))
                       (a (- o ma))
                       (side 'ahead)
                       (dist (dist (list x y) (list sx sy)))
                       (hit-dist (abs (* (sin (deg->rad a)) dist)))
                       (fa (chunk-slot-value my-ship angle))
                       (fd (chunk-slot-value my-ship dist))

                       (hit-f-dist (abs (* (sin (deg->rad fa)) fd))))

                  (when (> a 180)
                    (setf a (- a 360)))

                  (when (< a -180)
                    (setf a (+ a 360)))

                  (unless (<= -90 a 90)
                    (setf side 'behind))

                  (mod-chunk-fct 'my-ship (list 'mine-side side))

                  (mod-chunk-fct 'mine (list 'x x 'y y))
                  (mod-chunk-fct 'mine-loc (list 'screen-x x 'screen-y y))

                  (mod-chunk-fct 'my-ship (list 'mine-dist dist 'shoot-at-mine (<= hit-dist 20) 'hit-fortress-first (and (<= hit-f-dist 18) (< fd dist))))

                  (setf (mine-visible world) 12))))

        (t (print-warning "unexpected screen-type: ~s" data)))


      ;; Do some special processing for some of the visual information

      ;; hit hex is a bounce detector which keeps this set for a few
      ;; ticks so the model can detect it, but currently the ship doesn't
      ;; bounce so this being set just happens when the ship explodes

      (when (hit-hex world)
        (mod-chunk-fct 'my-ship (list 'hex-hit t)))

      ;; a fortress-hit stays "visible" for 3 ticks ~100ms
      ;; may also want to use an audio cue for that too since
      ;; the game does make a sound, but for now just keeping
      ;; it visual

      (when (numberp (f-hit-counter world))
        (incf (f-hit-counter world))

        (if (or (not (fast-chunk-slot-value-fct 'fortress-loc 'visible)) (= (f-hit-counter world) 4))
            (setf (f-hit-counter world) nil)
          (mod-chunk-fct 'fortress-hit-loc (list 'visible t))))


      ;; there should be fortress but it's not visible

      (when (and (fortress-there world) (not (fast-chunk-slot-value-fct 'fortress-loc 'visible)))

        ;; count the kill
        (unless (fortress-killed world)
          (incf (second (first (game-stats world))))
          (setf (fortress-killed world) t))

        (mod-chunk-fct 'fortress-explosion-loc (list 'visible t)))

      ;; there should be a mine but its loc isn't visible
      ;; then flag it as a dead-mine

      (when (and (mine-visible world)
                 (null (fast-chunk-slot-value-fct 'mine-loc 'visible))
                 (not (zerop (mine-visible world))))
        (mod-chunk-fct 'mine-loc (list 'visible t 'kind 'dead-mine))
        (decf (mine-visible world)))




      ;; Schedule the next update based on the needed delay

      (schedule-event-relative delay 'update-world :destination :sf :module :sf :maintenance t :time-in-ms t)


      ;; Schedule the proc-display if necessary

      (unless (pending-procs (current-device-interface)) ;; don't schedule it if there is already one waiting because of the device interface
        (unless (= (last-proc-scheduled world) (mp-time-ms)) ;; don't schedule it if we've already scheduled one at this time
          (setf (last-proc-scheduled world) (mp-time-ms))

          (schedule-event-now 'proc-display :module :sf)))

      ;; Call the handlers for the key event hooks after the vision module updates (why after vision?)

      (when (key-change-events world) ;; don't care if there are hooks or not
                                      ;; because need to clear the events either way
        (schedule-event-after-module :vision 'check-key-hook-handler :maintenance t :module :sf :destination :sf :output nil)))))


(defun check-key-hook-handler (world)
  (if (pending-procs (current-device-interface))
      (schedule-event-after-module :vision 'check-key-hook-handler :maintenance t :module :sf :destination :sf :output nil)
    (dolist (x (key-change-events world))
      (cond ((eq :down (cdr x))
             (when (or (null (key-down-hook world))
                       (funcall (key-down-hook world) world (car x)))
               (setf (key-change-events world) (remove x (key-change-events world)))))
            ((eq :up (cdr x))
             (when (or (null (key-up-hook world))
                       (funcall (key-up-hook world) world (car x)))
               (setf (key-change-events world) (remove x (key-change-events world)))))))))


;;; functions to compute the simulation of the player shooting at the mine
;;; used by the imaginal module.

;;; Assume the model won't thrust.

;;; Compute the number of game ticks it would take to hit the mine
;;; - if it doesn't turn
;;; - if it turns to the side the mine is on
;;;      compute the shortest to hit and the number of angle steps
;;;
;;;
;;; Assume there's a 9 tick delay before any turning will occur and that
;;; the my-ship and mine chunks hold the necessary info.
;;;
;;; If it can shoot the mine without turning in less that 5 ticks
;;; it won't bother to compute the others and just go with that.
;;;

(defun predict-mine-shooting ()
  (let* ((vx (chunk-slot-value my-ship vx))
         (vy (chunk-slot-value my-ship vy))
         (mx (chunk-slot-value mine x))
         (my (chunk-slot-value mine y))
         (o (chunk-slot-value my-ship orientation))
         (sx (chunk-slot-value my-ship x))
         (sy (chunk-slot-value my-ship y))
         (side nil)
         (fa (chunk-slot-value my-ship angle))
         (fd (chunk-slot-value my-ship dist))
         (no-turn-steps nil)
         (turn-steps nil)
         (turns nil)
         (d-hit-dist nil)
         (t-hit-dist nil))

    ;;; compute the number of steps needed to drift

    (do* ((count 0 (1+ count))
          (csx sx (+ csx vx))
          (csy sy (- csy vy))
          (mh 0 (atan (- cmy csy) (- csx cmx)))
          (cmx mx (+ cmx (* (cos mh) 5)))
          (cmy my (- cmy (* (sin mh) 5)))
          (cfa fa (- o (rad->deg (atan (- csy 315) (- 355 csx)))))
          (cfd fd (dist (list csx csy) '(355 315)))

          (hit-list (multiple-value-list (can-shoot-mine cmx cmy csx csy o cfa cfd))
                    (multiple-value-list (can-shoot-mine cmx cmy csx csy o cfa cfd)))
          (hit (car hit-list) (car hit-list))
          (done (second hit-list) (second hit-list)))
         ((or hit done) (when hit (setf no-turn-steps count)
                          (setf d-hit-dist (dist (list cmx cmy) (list csx csy)))))
      )

    (if (and no-turn-steps (<= no-turn-steps 5))
        (set-chunk-slot-value mine-predict action drift)

      ;; compute the steps if the model turns
      (do* ((count 0 (1+ count))
            (csx sx (+ csx vx))
            (csy sy (- csy vy))
            (mh 0 (atan (- cmy csy) (- csx cmx)))
            (cmx mx (+ cmx (* (cos mh) 5)))
            (cmy my (- cmy (* (sin mh) 5)))

            (o o)
            (cfa fa)
            (cfd fd (dist (list csx csy) '(355 315)))
            (hit-list (multiple-value-list (can-shoot-mine cmx cmy csx csy o cfa cfd)))
            (hit (car hit-list))
            (done (second hit-list)))
           ((or hit done)
            (when hit
              (setf turn-steps count)
              (setf t-hit-dist (dist (list cmx cmy) (list csx csy)))))

        (if (<= count 9)
            (multiple-value-bind (h d) (can-shoot-mine cmx cmy csx csy o cfa cfd)
              (declare (ignore h))
              (setf ; hit h if it would shoot in less than 9 it's already covered
                    ; and otherwise I need to recompute the aim angle to check the hit
               done d))

          (do* ;; iterate from 1 to 30 turns (or max that could have been made)
               ;; check to see if any hit or ship has been hit
               ;; if so return and then have those values recorded...
               ((turn 0 (1+ turn))
                (co-left (+ o (* turn 6)) (+ o (* turn 6)))
                (co-right (- o (* turn 6)) (- o (* turn 6)))
                (tfa-left (- co-left (rad->deg (atan (- csy 315) (- 355 csx))))
                          (- co-left (rad->deg (atan (- csy 315) (- 355 csx)))))
                (tfa-right (- co-right (rad->deg (atan (- csy 315) (- 355 csx))))
                           (- co-right (rad->deg (atan (- csy 315) (- 355 csx)))))
                (t-hit-list-left (multiple-value-list (can-shoot-mine cmx cmy csx csy co-left tfa-left cfd))
                                 (multiple-value-list (can-shoot-mine cmx cmy csx csy co-left tfa-left cfd)))
                (t-hit-list-right (multiple-value-list (can-shoot-mine cmx cmy csx csy co-right tfa-right cfd))
                                  (multiple-value-list (can-shoot-mine cmx cmy csx csy co-right tfa-right cfd)))
                (t-hit (or (car t-hit-list-left) (car t-hit-list-right))
                       (or (car t-hit-list-left) (car t-hit-list-right)))
                (t-done (or (second t-hit-list-left) (second t-hit-list-right))
                        (or (second t-hit-list-left) (second t-hit-list-right))))
               ((or t-hit t-done (> turn (- count 9)) (> turn 30))
                (when (or t-hit t-done)
                  (setf hit t-hit
                    done t-done
                    side (if (car t-hit-list-left) 'left 'right))
                  (when t-hit
                    (setf turns turn))))))))

    (cond ((null (or no-turn-steps turn-steps))
           ;; can't hit it in time...
           (set-chunk-slot-value mine-predict action panic)
           )
          ((and no-turn-steps turn-steps)
           ;; hit it with either -- give a preference to drifting
           ;; if it's close in time but if turning is a clear
           ;; winner go with it
           (if (<= no-turn-steps (+ turn-steps 3))
               (progn
                 (set-chunk-slot-value-fct 'mine-predict 'ticks (compute-ticks no-turn-steps))
                 (set-chunk-slot-value mine-predict action drift)
                 (set-chunk-slot-value-fct 'mine-predict 'dist (+ d-hit-dist 40))
                 (set-chunk-slot-value-fct 'mine-predict 'hit-dist (+ d-hit-dist -90))

                 ) ;; give it about 5 extra ticks
             (progn
               (set-chunk-slot-value mine-predict action turn)
               (if (eq side 'right)
                   (set-chunk-slot-value mine-predict finger index)
                 (set-chunk-slot-value mine-predict finger ring))
               (set-chunk-slot-value-fct 'mine-predict 'ticks (compute-ticks turn-steps))
               (set-chunk-slot-value-fct 'mine-predict 'delay (* turns .033))
               (set-chunk-slot-value-fct 'mine-predict 'hit-dist (+ t-hit-dist -90))
               (set-chunk-slot-value-fct 'mine-predict 'dist (+ t-hit-dist 40 (* 5 turns))) ;; buffer of 5 ticks (of mine speed) plus the turn time
                                                                                                 ;; with a slack for the event that the ship and mine
                                                                                                 ;; are moving toward each other.
               )))
          (no-turn-steps
           (set-chunk-slot-value mine-predict action drift)
           (set-chunk-slot-value-fct 'mine-predict 'ticks (compute-ticks no-turn-steps))
           (set-chunk-slot-value-fct 'mine-predict 'hit-dist (+ d-hit-dist -90))
           (set-chunk-slot-value-fct 'mine-predict 'dist (+ d-hit-dist 40)))
          (turn-steps
           (set-chunk-slot-value mine-predict action turn)
           (set-chunk-slot-value-fct 'mine-predict 'ticks (compute-ticks turn-steps))
           (if (eq side 'right)
               (set-chunk-slot-value mine-predict finger index)
             (set-chunk-slot-value mine-predict finger ring))
           (set-chunk-slot-value-fct 'mine-predict 'delay (* turns .033))
           (set-chunk-slot-value-fct 'mine-predict 'hit-dist (+ t-hit-dist -90))
           (set-chunk-slot-value-fct 'mine-predict 'dist (+ t-hit-dist 40 (* 5 turns))))
          (t (model-warning "Prediction code failed...")))


    (schedule-set-buffer-chunk 'imaginal 'mine-predict .2 :module 'imaginal)

    (schedule-event-relative .2 'set-imaginal-free :module 'imaginal)))

(defun compute-ticks (steps)
  (let ((time (* steps .033)))
    (1+ (ceiling (1- (log (1+ (/ time .11)) 1.1))))))

;; returns 2 values: ship-can-shoot-mine and mine-hits-ship

(defun can-shoot-mine (mx my sx sy o fa fd)
  (if (or (< sx 0) (< sy 0) (> sx 710) (> sy 626))
      (values nil t)
    (let* ((ma (rad->deg (atan (- sy my) (- mx sx))))
           (a (- o ma))
           (adjust-a (if (> a 180)
                         (- a 360)
                       (if (< a -180)
                           (+ a 360)
                         a)))

           (mdist (dist (list mx my) (list sx sy)))
           (hit-dist (abs (* (sin (deg->rad a)) mdist)))
           (shoot (and (<= hit-dist 20) (< (abs adjust-a) 90)
                   (not (and ;(fort-exists (current-device))
                         (< fd mdist)
                         (<= (abs (* (sin (deg->rad fa)) fd)) 18)))))
           (done (< mdist 30)))
      (values shoot done))))


;;; compute turn and thrust to get back into orbit after
;;; handling a mine
;;;
;;; Using a speed based correction
;;;  - low speed (< 1)
;;;    turn out to the tangent angle
;;;    give a fast thrust
;;;  - medium speed (1 - 1.5)
;;;    turn to 1/2 tangent angle
;;;    give a fast thrust
;;;  - high speed (1.5 - 1.9)
;;;    turn to 0
;;;  - give a default thrust
;;;  - very high speed > 1.9
;;;    turn to - tangent angle
;;;  - give a default thrust
;;;
;;; Assume that the model is already turning to
;;; near 0 if the |angle| is > 20.

(defun reorbit-from-dist ()

  (let* ((h (chunk-slot-value my-ship dist))
         (angle (chunk-slot-value my-ship angle))
         (r 90)
         (speed (chunk-slot-value my-ship vel))
         (theta (rad->deg (asin (/ r h))))
         (thrust-len (if (< speed 1.5) 'fast 'default))
         (target-angle (cond ((< speed 1) theta)
                             ((< speed 1.5) (/ theta 2))
                             (t 0))))

    (unless (<= -20 angle 20)
      (setf angle 0))

    (let ((delta-angle (- angle target-angle))
          (turn-time nil)
          (turn-finger nil))

      (when (> delta-angle 6)
        (setf turn-time (* .033 (round (abs delta-angle) 6)))
        (setf turn-finger (if (plusp delta-angle) 'index 'ring)))

      (set-chunk-slot-value-fct 'reorbit 'turn turn-time)
      (set-chunk-slot-value-fct 'reorbit 'finger turn-finger)

      (set-chunk-slot-value-fct 'reorbit 'thrust thrust-len)

      (schedule-set-buffer-chunk 'imaginal 'reorbit .2 :module 'imaginal)

      (schedule-event-relative .2 'set-imaginal-free :module 'imaginal))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some function to handle trig info (some of these come over directly now so
;;; and don't need to be computed on the model side).

(defun vdir (x y vx vy)
  (let* ((dir (atan vy vx))
         (actual (to-fortress-orient x y))
         (diff (rad->deg (- dir actual))))
    (if (> diff 180)
        (- diff 360)
      (if (< diff -180)
          (+ diff 360)
        diff))))


(defun angle-to-fortress (x y orient)
  (let* ((actual (rad->deg (to-fortress-orient x y)))
         (diff (- orient actual)))
        (if (> diff 180)
        (- diff 360)
      (if (< diff -180)
          (+ diff 360)
        diff))))

(defun to-fortress-orient (x y)
  (let ((dx (- 355 x))
        (dy (- y 315)))
    (atan dy dx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The module to provide the interface, parameters, etc

(defun create-sf-module (name)
  (declare (ignore name))
  (make-instance 'sf-world))

(defun pre-reset-sf-module (world)

  ;; Kill any previous connection

  (when (sf-connection world)
    (setf (sf-connection world) nil))

  ;; clear out the slots as needed

  (setf (game-state world) 'disconnected)
  (clrhash (loc->obj-table world))

  (setf (last-proc-scheduled world) -1)
  (setf (data-record world) nil)

  (setf (f-hit-counter world) nil)
  (setf (hit-hex world) nil)
  (setf (last-v world) nil)
  (setf (mine-visible world) nil)

  (setf (responses world) nil)
  (setf (auto-response world) nil)

  (setf (key-change-events world) nil)

  ;; hack to make the fortress always visible
  ;; since that's the case for this task
  ;; and there are no condition screens

  (setf (fort-exists world) t)

  (setf (ship-alive world) nil)

  (setf (fortress-there world) nil)
  (setf (last-time world) 0)

  ;; track deaths and kills per game
  (setf (game-stats world) (list (list 0 0)))

  ;; name for the data file
  (setf (name world) nil)

  ;; track complete games
  (setf (game-count world) 0)
  (setf (game-limit world) -1)
  (setf (change-drawing-mode world) nil))


(defun post-reset-sf-module (world)

  ;; create custom chunk-types for the features and objects

  (chunk-type (sf-visual-location (:include visual-location)) visible)

  (chunk-type (sf-object (:include visual-object)) label)

  (chunk-type (ship (:include sf-object)) (ship t) (label ship)
              x y angle vel dist vdir hex-dist orientation last-dist hex-hit mine-side mine-dist shoot-at-mine hit-fortress-first vx vy speed-change travel-time-to-bighex travel-time-to-smallhex dist-to-bighex dist-to-smallhex travel-dist-to-bighex travel-dist-to-smallhex)
  (chunk-type (mine (:include sf-object)) x y (label mine))
  (chunk-type (fortress (:include sf-object)) angle (label fortress))
  (chunk-type (bonus (:include sf-object)) (bonus t) (label bonus))
  (chunk-type (trial-end (:include sf-object)) (trial-end t) (label trial-end))
  (chunk-type (projectile (:include sf-object)) x y vx vy orientation count)
  (chunk-type (missile (:include projectile)) (label missile))
  (chunk-type (shell (:include projectile)) (label shell))
  (chunk-type (labeled-text (:include sf-object)(:include text)))
  (chunk-type (explosion (:include sf-object)) (value explosion))
  (chunk-type (fortress-explosion (:include explosion)) (label fortress-explosion))
  (chunk-type (ship-explosion (:include explosion)) (label explosion))
  (chunk-type (fortress-hit (:include sf-object)) (label fortress-hit))

  ;; some types for the calculations that 'cheat' through an imaginal action

  (chunk-type mine-heading action finger delay ticks dist hit-dist)
  (chunk-type reorbit turn thrust finger)

  ;; specific chunks for the features and objects to keep things simple

  (define-chunks (ship isa chunk)
    ;; features
    (own-ship-loc isa sf-visual-location kind ship)
    (explosion-loc isa sf-visual-location kind explosion)
    (fortress-loc isa sf-visual-location kind fortress screen-x 355 screen-y 315)
    (fortress-explosion-loc isa sf-visual-location kind fortress-explosion screen-x 355 screen-y 315)
    (bonus-loc isa sf-visual-location kind bonus screen-x 355 screen-y 420)
    (vlner-loc isa sf-visual-location screen-x  465 screen-y 700 kind text)
    (pnts-loc isa sf-visual-location screen-x  245 screen-y 700 kind text)
    (fortress-hit-loc isa sf-visual-location kind fortress-hit screen-x 355 screen-y 315)
    (trial-end-loc isa sf-visual-location kind trial-end screen-x 355 screen-y 315)
    (foe1-loc isa sf-visual-location screen-x 200 screen-y 500 kind text)
    (foe2-loc isa sf-visual-location screen-x 300 screen-y 500 kind text)
    (foe3-loc isa sf-visual-location screen-x 400 screen-y 500 kind text)
    (cond1-loc isa sf-visual-location screen-x 200 screen-y 500 kind text)
    (cond2-loc isa sf-visual-location screen-x 400 screen-y 500 kind text)
    (iff-loc isa sf-visual-location screen-x 600 screen-y 700 kind text)
    (mine-loc isa sf-visual-location kind mine)
    (missile-loc isa sf-visual-location kind missile)
    (shell-loc isa sf-visual-location kind shell)

    ;; objects
    (my-ship isa ship)
    (explosion isa ship-explosion)
    (fortress isa fortress)
    (fortress-explosion isa fortress-explosion)
    (bonus isa bonus)
    (vlner isa labeled-text value 0 label vlner)
    (pnts isa labeled-text value 0 label points)
    (fortress-hit isa fortress-hit)
    (shell isa shell value shell)
    (missile isa missile value missile)
    (trial-end isa trial-end)

    ;; objects not used in the current simplified games
    (foe1 isa labeled-text label foe)
    (foe2 isa labeled-text label foe)
    (foe3 isa labeled-text label foe)
    (cond1 isa labeled-text label condition)
    (cond2 isa labeled-text label condition)
    (iff isa labeled-text label iff)
    (mine isa mine)

    ;; 'tag' chunks used above and internal calculation chunks

    (mine-predict isa mine-heading)
    (reorbit isa reorbit)
    (new-trial-end) (foe) (condition)
    (debris) (inner) (outer)(points))

  ;; create all the visual-location and visual-object
  ;; chunks up front and set up the mappings - assuming
  ;; the locs and objs are created in corresponding order

  (setf (dynamics world)
    (list 'own-ship-loc 'explosion-loc 'fortress-loc 'fortress-explosion-loc 'bonus-loc 'fortress-hit-loc 'trial-end-loc 'vlner-loc 'pnts-loc
          'foe1-loc 'foe2-loc 'foe3-loc 'cond1-loc 'cond2-loc 'iff-loc 'mine-loc 'missile-loc 'shell-loc))

  (setf (all-locs world) (dynamics world))

  (mapc (lambda (loc obj) (setf (gethash loc (loc->obj-table world)) obj))
    (all-locs world)
    (list 'my-ship 'explosion 'fortress 'fortress-explosion 'bonus 'fortress-hit 'trial-end 'vlner 'pnts 'foe1 'foe2 'foe3 'cond1 'cond2 'iff 'mine 'missile 'shell))

  ;; Set :tracking-clear to nil so it re-encodes explosions

  (sgp :tracking-clear nil)

  ;; automatically make this the current device

  (install-device world))


(defun delete-sf-module (world)
  ;; Kill any open connections

  (when (sf-connection world)
    (setf (sf-connection world) nil)))

;; respond to the game buffer queries

(defun sf-query (world buffer-name slot value)
  (case buffer-name

    (game (case slot
            (state
             (case value
               (busy nil)
               (free t)
               (error nil)
               (t (print-warning "Unknown state query ~S to game buffer" value)
                  nil)))
            (game-state
                (eq (game-state world) value))
            (t (print-warning "Unknown query ~S ~S to the game buffer" slot value))))))



;; custom function for printing the buffer-status of the game buffer

(defun print-sf-game-state ()
  (command-output "  game-state connecting  : ~S"
                  (query-buffer 'game
                                '(game-state connecting)))
  (command-output "  game-state connected   : ~S"
                  (query-buffer 'game
                                '(game-state connected)))
  (command-output "  game-state disconnected: ~S"
                  (query-buffer 'game
                                '(game-state disconnected))))


;; function to handle the setting and getting of the module's parameters

(defun sf-params (world param)
  (cond ((consp param)

          (case (car param)
            (:sf-connection (setf (sf-connection world) (cdr param)))
            (:log-sf-data-in (setf (collect-incoming world) (cdr param)))
            (:log-sf-data-out (setf (record-outgoing world) (cdr param)))
            (:sf-fixation-hook (setf (fix-hook world) (cdr param)))
            (:sf-start-hook (setf (start-hook world) (cdr param)))
            (:sf-end-hook (setf (end-hook world) (cdr param)))
            (:sf-key-down-hook (setf (key-down-hook world) (cdr param)))
            (:sf-key-up-hook (setf (key-up-hook world) (cdr param)))
            (:sf-data-hook (setf (data-hook world) (cdr param)))))
        (t
         (case param
           (:sf-connection (sf-connection world))
           (:log-sf-data-in (collect-incoming world))
           (:log-sf-data-out (record-outgoing world))
           (:sf-fixation-hook (fix-hook world))
           (:sf-start-hook (start-hook world))
           (:sf-end-hook (end-hook world))
           (:sf-key-down-hook (key-down-hook world))
           (:sf-key-up-hook (key-up-hook world))
           (:sf-data-hook (data-hook world))))))


;; define the module itself

(define-module-fct :sf '((game nil nil (game-state) print-sf-game-state))
  (list
   ;; (define-parameter :sf-host :valid-test #'stringp :default-value *host-address*
   ;;   :warning "a string" :documentation "IP address or host name of SF server")
   ;; (define-parameter :sf-port :valid-test 'integerp  :default-value *port-for-server*
   ;;   :warning "an integer" :documentation "Gamebots port of the server for connecting")
   (define-parameter :log-sf-data-in :valid-test #'tornil :default-value nil
     :warning "T or nil" :documentation "Record all incoming SF data")
   (define-parameter :log-sf-data-out :valid-test #'tornil :default-value nil
     :warning "T or nil" :documentation "Record all commands sent to SF")
   (define-parameter :sf-fixation-hook :valid-test #'fctornil
     :default-value nil
     :warning "a function or nil" :documentation "Middle of fixation hook")
   (define-parameter :sf-start-hook :valid-test #'fctornil
     :default-value nil
     :warning "a function or nil" :documentation "Game start hook")
   (define-parameter :sf-end-hook :valid-test #'fctornil
     :default-value nil
     :warning "a function or nil" :documentation "Game end hook")

   (define-parameter :sf-key-up-hook :valid-test #'fctornil
     :default-value nil
     :warning "a function or nil" :documentation "Key up after display processed")
   (define-parameter :sf-key-down-hook :valid-test #'fctornil
     :default-value nil
     :warning "a function or nil" :documentation "Key down after display processed")
   (define-parameter :sf-data-hook :valid-test #'fctornil
     :default-value nil
     :warning "a function or nil" :documentation "Called with each data line received from the game interface"))

  :version "5.0"
  :documentation "Module to allow ACT-R 7 to connect to SpaceFortress with our custom socket interface"
  :creation 'create-sf-module
  :reset (list 'pre-reset-sf-module 'post-reset-sf-module)
  :delete 'delete-sf-module
  :query 'sf-query
  :params 'sf-params)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Handle all the config info. for the current game servers when a new session gets
;;; started with one of the running functions
;;;
;;; - read the first config line
;;; - send draw and id settings verifying that the result is t for each
;;;
;;; If anything doesn't work out punt and close the connection

(defun same-id-test (requested returned)
  (unless (stringp requested)
    (setf requested (princ-to-string requested)))
  (string-equal requested returned))

(defun connect-to-game (module draw name)
  (if (not (eq (game-state module) 'disconnected))
      (print-warning "Model is already connected cannot connect.")
      (progn
        (setf (game-state module) 'connecting)
        (let ((n (if name
                     name
                     (let (number)
                       (if (probe-file (translate-logical-pathname "SF-DATA:sf-model-number.txt"))
                           (with-open-file (f (translate-logical-pathname "SF-DATA:sf-model-number.txt") :direction :input)
                             (setf number (ignore-errors (read f nil :done))))
                           (setf number -1))
                       (if (numberp number)
                           (incf number)
                           (setf number 0))

                       (with-open-file (f (translate-logical-pathname "SF-DATA:sf-model-number.txt") :direction :output :if-exists :supersede :if-does-not-exist :create)
                         (write number :stream f))
                       (format nil "model_~d" number)))))
          (setf (name module) n))
        (handler-case
            (progn
              (setf (sf-connection module) (sf-connection:connection-init (name module) draw))
              (when (sf-connection module)
                (setf (game-state module) 'connected)))
          (sf-connection:connection-error (c)
            (setf (game-state module) 'disconnected)
            (print-warning "~A" c)
            nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for actually playing the game -- call these to run the model.
;;;
;;; The game can be run for a specific time in seconds using the play-sf function
;;; or a fixed number of games using the play-sf-games function.  Each of those
;;; has one required parameter for the indicated value (time or games).
;;;
;;; They also have 4 keyword parameters:
;;;
;;; :speed should be a number and indicates how fast to run the game relative
;;;        to real-time.  The default is 1 which means at real time.  A bigger
;;;        value runs the model faster than real time and a smaller value slower.
;;;
;;; :cont whether the current run should be continuted.  If this is specified
;;;       as t then it does not reconnect the model before starting running
;;;       and if it is nil then it resets the model and starts a new session.
;;;       The default is nil.
;;;
;;; :draw can be t, nil, or :debug.  Indicates whether the game server displays
;;;       the game as the model plays.  A value of t means show everything and nil
;;;       means show nothing.  For some versions of the server :debug causes a
;;;       very limited display (sufficient to see that something is happening and
;;;       it's not hung), but not all support that mode in which case it will
;;;       show everything like t does.  The default value is t.
;;;
;;; :name should be a symbol, string, or number which is used to create a directory
;;;       in the game's data folder for the model's game data files.  If it is not
;;;       provided then the name will be "model_#" where # is one more than the
;;;       number read from the sf-model-number.txt file found in the same directory
;;;       as the one from which this file was loaded or the number 0 if that file
;;;       is not found.  When it generates a number to use it will write that number
;;;       to the sf-model-number.txt file (creating the file if it does not exist).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Game now runs in real-time mode to allow for speed control (doesn't use a custom
;;; clock anymore to simplify things).

(defun play-sf (time &key (speed 1) (cont nil) (draw t) name)
  (let ((module (get-module :sf)))
    (if module
        (progn
          (if cont
              (progn
                (setf (change-drawing-mode module) (if draw (if (eq draw :debug) 1 2) 0))
                (run-full-time time :real-time speed))
            (progn
              (reset)
              (mp-real-time-management :allow-dynamics t)
              (when (connect-to-game module draw name)
                (schedule-event 0 'update-world :destination :sf :module :sf :priority :max :time-in-ms t)
                (run-full-time time :real-time speed)))))
      (print-warning "Cannot play space fortress"))))


(defun check-game-count (module)
  ;; for safety allow it to have run over to catch for a bad update,
  ;; spurious result from the server, or other unforseen issue.

  (>= (game-count module) (game-limit module)))

(defun play-sf-games (games &key (speed 1) (cont nil) (draw t) name)

  (let ((module (get-module :sf)))
    (if module
        (progn
          (if cont
              (progn
                (setf (game-count module) 0)
                (setf (game-limit module) games)
                (setf (change-drawing-mode module) (if draw (if (eq draw :debug) 1 2) 0))
                (run-until-condition (lambda () (check-game-count module)) :real-time speed))
            (progn
              (reset)
              (setf (game-count module) 0)
              (setf (game-limit module) games)
              (mp-real-time-management :allow-dynamics t)
              (when (connect-to-game module draw name)
                (schedule-event 0 'update-world :destination :sf :module :sf :priority :max :time-in-ms t)
                (run-until-condition (lambda () (check-game-count module)) :real-time speed)))))
      (print-warning "Cannot play space fortress"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load all the other modules and code that this depends on once and create a logical
;;; directory called SF-DATA pointing to the directory this came from.
;;;

(unless (boundp '*loaded-modules*)
  (defparameter *loaded-modules* t)
  (compile-and-load "ACT-R:extras;extended-motor-actions;motor-extension.lisp")
  (compile-and-load "ACT-R:space-fortress-modules;delayed-punch.lisp")
  (compile-and-load "ACT-R:space-fortress-modules;hold-until.lisp")
  (compile-and-load "ACT-R:space-fortress-modules;attend-and-track.lisp")
  (compile-and-load "ACT-R:space-fortress-modules;visual-search-buffer.lisp")
  (compile-and-load "ACT-R:space-fortress-modules;analyze-position.lisp")

  (setf (logical-pathname-translations "SF-DATA")
    `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" *load-truename*))))))


#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
