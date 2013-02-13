#ifndef Player_h
#define Player_h

#include "C4Application.h"
#include "C4Character.h"
#include "C4World.h"
#include "C4Engine.h"

#include "Cameras.h"
#include "Constants.h"
#include "Input.h"

namespace C4 {

	// This is the type for the controller that we use to move the player.
	enum {
		kControllerPlayer		= 'sold'
	};

	// an enumeration of the inputs player takes
	enum {
		kInputNone      = 0,
		kInputUp		= 1 << 0,
		kInputDown		= 1 << 1,
		kInputLeft		= 1 << 2,
		kInputRight		= 1 << 3,
		kInputJump		= 1 << 4,
		kInputSprint	= 1 << 5,

		kInputPlanarMask	= (1<<6) - 1
	};


	///////////////////////////////////////////////////////////////////////////////////
	///   State enums - Left is always first and stores flipped (aka facing left bool)
	///////////////////////////////////////////////////////////////////////////////////

	// an enumeration for turning flags
	enum {
		kTurningNormal  = 0,
		kTurningQuick	= 1
	};

	// an enumeration for jumping flags
	enum {
		kJumpingVert,
		kJumpingStand,
		kJumpingSprint
	};

	// an enumeration for the movement flags
	enum {
		kMovingSprint  	  = 1 << 0,
		kMovingCollision  = 1 << 1,
		kMovingStopping   = 1 << 2,

		kMovingPlanarMask = (1<<3) - 1
	};

	
	// These are motion states that are used with the player controller to keep track
	// of which animation should be played.
	enum PlayerAnim {
		kAnimNone,
		kAnimIdle,
		kAnimWalk,
		kAnimRun,
		kAnimJump,
		kAnimJumpUp,
		kAnimFalling,
		kAnimHanging,
		kAnimClimbUp,
		kAnimHangFalling,
		kAnimRope,
		kAnimRopeUp,
		kAnimRopeDown,
		kAnimDead,
		kAnimClimb,
		kAnimStop
	};
	

	// The current state the player (what type of movement)
	// equal to the key combo to start the action (none listed combinations are ignored)
	enum PlayerState {
		kStateNone,
		kStateFalling,
		kStateHangFalling,
		kStateJumping,
		kStateJumpingUp,
		kStateClimbUp,
		kStateHanging,
		kStateTurning,
		kStatePivoting,
		kStateRope,
		kStateRopeTurn,
		kStateDead,
		kStateRespawn
	};

	// Controllers are used to control anything that moves in the world.
	// New types of controllers defined by the application/game module can be
	// registered with the engine when the Game class is constructed.
	// This particular controller is used to move and animate the player.
	
	class PlayerController : public CharacterController {
		friend class StateHelpers;
		private:
			// The input flags tell which buttons are pressed
			unsigned long	inputFlags;
			
			// The player state tracks what the player is currently doing
			PlayerState		currState;
			unsigned long	currStateFlags;

			// The player motion keeps track of what animation is currently playing.
			PlayerAnim		playerAnim;
			
			// The azimuth and altitude represent the direction the player is looking
			// by using the mouse.
			float				playerAzimuth;
			float				playerAltitude;

			// variables for tracking things which require
			Point3D				startPos;
			float				startTime;
			float				startAzm;

			int					pivotDir;
			bool				onPivot;
			bool				hasPivoted;

			Point3D				hangPos;
			long				climbTime;

			Point3D				moverPos;

			// where the player model is originally attatched
			Node*				rootNode;

			// rotation of the model
			float				modelAzimuth;

			// easy way to determine which way it should be facing
			bool				facingLeft;
			
			// The frame animator controls playback of an animation resource.
			FrameAnimator		*frameAnimator;

			//Last checkpoint the character reached
			Point3D				lastCheckpoint;

			
			void SetAnim(PlayerAnim anim);
			void Spawn(void);

			long coinAmount;
			bool monkeyParts[3];
		
		public:
			PlayerController(const Point3D& position);
			~PlayerController();
			
			Entity *GetTargetNode(void) const {
				return (static_cast<Entity *>(CharacterController::GetTargetNode()));
			}
			
			unsigned long GetInputFlags(void) const {
				return (inputFlags);
			}
			
			void SetInputFlags(unsigned long flags) {
				inputFlags = flags;
			}
			
			float GetPlayerAzimuth(void) const {
				return (playerAzimuth);
			}

			void SetPlayerAzimuth(float m) {
				playerAzimuth = m;
			}

			float GetModelAzimuth(void) const {
				return (modelAzimuth);
			}

			Point3D GetLastCheckpoint(void) const {
				return (lastCheckpoint);
			}

			void SetLastCheckpoint(Point3D newCheckpoint){
				lastCheckpoint.x = newCheckpoint.x;
				lastCheckpoint.y = newCheckpoint.y;
				lastCheckpoint.z = newCheckpoint.z;
			}

			bool IsFacingLeft(void) {
				return (facingLeft!=0);
			}
			
			void Preprocess(void);
			
			void Move(void);
			void Travel(void);

			void StopPlayer () {
				playerAnim = kAnimNone;
				inputFlags = 0;
	
				currState = kStateNone;
				currStateFlags  = 0;
			}

			long GetCoin(void) const { return (coinAmount); } 
			void AddCoin(long amount) { coinAmount += amount; } 
			void AddMonkey(int index) { monkeyParts[index] = true; }
			bool GetMonkey(int index) const { return monkeyParts[index]; } 

			void SetPivotDir(int dir) { pivotDir = dir; }
			void OnPivot(void) { onPivot = true; }
			void Pivot(void) { if(currState == kStateNone) { currState = kStatePivoting; startAzm = playerAzimuth; hasPivoted = !hasPivoted;}}
			void Grip(void) { 
				if(currState != kStateNone) {
					currState = kStateHanging; 
					hangPos = GetTargetNode()->GetWorldPosition(); 
					inputFlags = 0;
				}
			}

			void GrabRope(void) { 
				if(currState != kStateNone) {
					currState = kStateRope; 
					if(facingLeft) 
						hangPos = GetTargetNode()->GetWorldPosition() + Vector3D(Cos(playerAzimuth)*-.3f, Sin(playerAzimuth)*-.3f, 0.f); 
					else
						hangPos = GetTargetNode()->GetWorldPosition() + Vector3D(Cos(playerAzimuth)*.3f, Sin(playerAzimuth)*.3f, 0.f); 
					inputFlags = 0;
				}
			}
			
			void KillPlayer(void) {
				currState = kStateDead;
				inputFlags = 0;

				SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
				side->SetOffsetXY(0);
				side->SetTrackType(kCamTrack);

			}

			void ResetNode(void) {
				GetTargetNode()->GetSuperNode()->RemoveSubnode(GetTargetNode());
				rootNode->AddSubnode(GetTargetNode());
			}
	};
}
#endif