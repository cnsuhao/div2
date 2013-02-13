
#include "Game.h"
#include "Player.h"
#include "Input.h"

#include "PropertyRegistrar.h"
#include "KillMover.h"

#include "StateHelpers.h"

using namespace C4;

PlayerController::PlayerController(const Point3D& position) : CharacterController(kControllerPlayer, 0.33F, 1.8f, kCharacterGravity, position) { 
	// set friction	and gravity
	SetGroundResistanceCoefficient(.1f);
	SetMediumResistanceCoefficient(0.03f);
	SetGravityForce(Vector3D(0,0,K::gravity*3));

	coinAmount = 0; 
	monkeyParts[0] = false; 
	monkeyParts[1] = false; 
	monkeyParts[2] = false;

	Spawn();
}

void PlayerController::Spawn() {
	// animation and input flags
	playerAnim = kAnimNone;
	inputFlags = 0;
	
	// storage for current states
	currState = kStateNone;
	currStateFlags  = 0;

	// set player angles
	playerAltitude = 0.f;
	playerAzimuth = 0.f;
	
	// set model angles
	modelAzimuth = -K::pi_over_2;
	facingLeft = false;

	onPivot = false;
	hasPivoted = false;
	pivotDir = 0;
}


PlayerController::~PlayerController() {}

/**
 * Initialize the player controller, animation hangling may be moved
 */
void PlayerController::Preprocess(void) {
	CharacterController::Preprocess();
	
	// Create the models animator
	Entity *model = GetTargetNode();
	frameAnimator = new FrameAnimator(model);
	model->SetRootAnimator(frameAnimator);

	rootNode = model->GetSuperNode();
}




/**
 * movement code updates player movement with relevant information
 * main types of movement are hanging, jumping, turning, and walking
 * this code updates what travel needs
 */
void PlayerController::Move(void) {

	///////////////////////////////////////////////////////////////
	///   Initialize  - set up variables, assure there is a state
	///////////////////////////////////////////////////////////////

	CharacterController::Premove();

	// initialize the motion and propeling force
	Node* model = GetTargetNode();
	PlayerAnim anim = kAnimIdle;
	Vector3D vel(0.f, 0.f, 0.f);
	Vector3D propel(0.f, 0.f, 0.f);
	long time = TheTimeMgr->GetDeltaTime();


	// figure out if the player is falling
	if ((GetCharacterFlags() & kCharacterGround) == 0 && currState != kStateJumping && currState != kStateJumpingUp && currState != kStateHangFalling && currState != kStateHanging && currState != kStateClimbUp && currState != kStateDead  && currState != kStateRope  && currState != kStateRopeTurn) {
		currState = kStateFalling;
		anim = kAnimFalling;
	}
	
	// update the player if they landed
	if ((GetCharacterFlags() & kCharacterGround) != 0) {
		if(currState == kStateJumping || currState == kStateJumpingUp || currState == kStateFalling || currState == kStateHangFalling) {
			if(onPivot && currState == kStateJumpingUp) {
				currState = kStateNone;
				Pivot();
			}
			else {
				currState = kStateNone;
			}
		}
	}

	// Get input
	bool up		= (inputFlags & kInputUp)	  != 0;
	bool down	= (inputFlags & kInputDown)	  != 0;
	bool left	= (inputFlags & kInputLeft)   != 0;
	bool right	= (inputFlags & kInputRight)  != 0;
	bool jump	= (inputFlags & kInputJump)	  != 0;
	bool sprint = (inputFlags & kInputSprint) != 0;

	float dAzm = playerAzimuth + modelAzimuth - (3*K::pi_over_2);
	float dx = Cos(dAzm) * K::playerMovementSpeed;
	float dy = Sin(dAzm) * K::playerMovementSpeed;
	Vector3D dv;


	///////////////////////////////////////////////////////////////////////////
	///   Update  - according to playerState, update the player for movement
	///////////////////////////////////////////////////////////////////////////
	switch(currState) {	

	/**
	 *  handle standard moves/inputs
	 */
	case kStateNone:
		//initialize common variables	
		/** jumping */
		if(jump) {
			onPivot = false;
			inputFlags &= ~kInputJump;
			currState = kStateJumping;
			anim = kAnimJump;

			dx =  Cos(dAzm) * K::playerJumpSpeed;
			dy =  Sin(dAzm) * K::playerJumpSpeed;

			if(sprint && (left ||right) && !(left && right)) { //sprint
				dv = Vector3D(dx*1.5, dy*1.5, K::playerHighJump);
			}
			else if (left || right) { //walking jump
				dv = Vector3D(dx*2.0,dy*2.0, K::playerLowJump);
			}
			else { //standing juump
				dv = Vector3D(dx*0.71f,dy*0.71f, K::playerLowJump);
			}
			
			unsigned long characterFlags = GetCharacterFlags();
			bool tjump = ((characterFlags & kCharacterGround) != 0);
			if ((!tjump) && (characterFlags & kCharacterJumpAllowed)) {
				CollisionData	data;
				
				Point3D p1(GetFinalPosition()+ Vector3D(0,0,GetRadius()));
				Point3D p2(GetFinalPosition() - Vector3D(0,0,0.25F));
				
				tjump = GetTargetNode()->GetWorld()->DetectCollision(p1, p2, GetRadius(), kColliderCharacter, &data);
				if (data.normal.z < GetMaxGravityGroundCosine()) tjump = false;
			}
			
			if (tjump) {
				SetCharacterFlags(characterFlags & ~(kCharacterGround | kCharacterJumpAllowed));
				vel = dv;
			}

			break;
		}
		else if(up && !(left || right || down)) {
			anim = kAnimJumpUp;
			currState = kStateJumpingUp;

			unsigned long characterFlags = GetCharacterFlags();
				bool jump = ((characterFlags & kCharacterGround) != 0);
				if ((!jump) && (characterFlags & kCharacterJumpAllowed)) {
					CollisionData	data;
					
					Point3D p1(GetFinalPosition()+ Vector3D(0,0,GetRadius()));
					Point3D p2(GetFinalPosition() - Vector3D(0,0,0.25F));
					
					jump = GetTargetNode()->GetWorld()->DetectCollision(p1, p2, GetRadius(), kColliderCharacter, &data);
					if (data.normal.z < GetMaxGravityGroundCosine()) jump = false;
				}
				
				if (jump) {
					SetCharacterFlags(characterFlags & ~(kCharacterGround | kCharacterJumpAllowed));
					vel = Vector3D(0,0, K::playerHighJump);
				}
			break;
		}

		/** invalid input */
		if(left == right) {
			break;
		}
		onPivot = false;

		/** turning */
		if(facingLeft && right) {
			currState		= kStateTurning;
			currStateFlags  = kTurningNormal;
			facingLeft		= !facingLeft;
			break;
		}
		else if (!facingLeft && left) {
			currState		= kStateTurning;
			currStateFlags  = kTurningNormal;
			facingLeft		= !facingLeft;
			break;
		}
		/** moving */
		if ((sprint && left) || (sprint && right)) {
			anim = kAnimRun;
			propel.x = dx * K::playerSprintingMult;
			propel.y = dy * K::playerSprintingMult;
			break;
		}

		if (left || right) {
			anim = kAnimWalk;
			propel.x = dx;
			propel.y = dy;
			break;
		}

		break;


	/**
	 *  handle situations where there is just an anim update
	 */
	case kStateTurning: 
		anim = kAnimIdle;
		break;

	case kStateFalling:
		anim = kAnimFalling;
		break;

	case kStateHangFalling:
		anim = kAnimHangFalling;
		break;

	case kStateClimbUp:
		anim = kAnimClimbUp;
		climbTime += time;
		break;

	case kStateJumping:
		anim = kAnimJump;
		break;

	case kStateJumpingUp:
		anim = kAnimJumpUp;
		break;

	case kStatePivoting:
		anim = kAnimIdle;
		break;

	case kStateDead:
		anim = kAnimDead;
		if(jump)
			currState = kStateRespawn;
		break;

	/**
	 * handle hanging
	 */
	case kStateHanging:
		anim = kAnimHanging;

		if(down) currState = kStateHangFalling;
		if(up) { currState = kStateClimbUp; climbTime = 0; }

		break;

	case kStateRope:
		anim = kAnimIdle;

		if(down) hangPos+=Vector3D(0,0,-1.e-2f);
		if(up)   hangPos+=Vector3D(0,0, 1.e-2f);

		if(jump) {
			model->SetNodePosition(model->GetSuperNode()->GetInverseWorldTransform() * hangPos);
			SetFinalPosition(hangPos);
			inputFlags &= ~kInputJump;
			currState = kStateJumping;
			anim = kAnimJump;

			dx =  Cos(dAzm) * K::playerJumpSpeed;
			dy =  Sin(dAzm) * K::playerJumpSpeed;

			long characterFlags = GetCharacterFlags();

			bool tjump = ((characterFlags & kCharacterGround) != 0);

			SetCharacterFlags(characterFlags & ~(kCharacterGround | kCharacterJumpAllowed));
			vel = Vector3D(dx*1.5,dy*1.5, K::playerHighJump * 1.5);

			break;
		}

		if(facingLeft && right) {
			currState		= kStateRopeTurn;
			currStateFlags  = kTurningNormal;
			facingLeft		= !facingLeft;
		}
		else if (!facingLeft && left) {
			currState		= kStateRopeTurn;
			currStateFlags  = kTurningNormal;
			facingLeft		= !facingLeft;
		}

		break;

	case kStateRopeTurn:
		anim = kAnimIdle;
		break;
	}

	//////////////////////////////////////////////////////////
	///   Finalize - Prepare the player for Travel function
	//////////////////////////////////////////////////////////
	
	// update velocity
	SetInitialState(GetFinalPosition(), GetVelocity(GetCharacterTime()) + vel);

	// update propulsion
	if (propel != GetPropulsionForce()) {
		SetInitialState(GetFinalPosition(), GetVelocity(GetCharacterTime()));
		SetPropulsionForce(propel);
	}

	// update the animation
	if (anim != playerAnim) SetAnim(anim);

	// finally, move the CharacterController
	CharacterController::Move(); 
}









void PlayerController::Travel(void) {
		
	CharacterController::Travel();

	// get the player model
	Entity *model = GetTargetNode();

	CollisionState state = GetCollisionState();
	const CollisionData *data = GetCollisionData();

	// activate triggers
	//    there should never be a scenario where final distance accidentally activates a trigger because it projected
	//    to far, it is safe to assume that if there is a collision, the Final position shouldn't activate soemthing
	//    on the other side of the object
	Point3D finalPos = GetFinalPosition();
	Point3D lastPos = model->GetNodePosition();
	model->GetWorld()->ActivateTriggers(lastPos, finalPos, GetRadius(), model);
	
	// update mover offset
	Point3D off(0.f,0.f,0.f);
	if (GetTargetNode()->GetSuperNode() != rootNode) {
		off = model->GetSuperNode()->GetWorldPosition() - moverPos;
		moverPos = model->GetSuperNode()->GetWorldPosition();
	}
	
	finalPos += off;


	// actually move
	if (state == kCollisionStateNone) {
		Node* super = model->GetSuperNode();
		model->SetNodePosition(super->GetInverseWorldTransform()*finalPos);
	}
	else {
		if(state == kCollisionStateGeometry) {
			if(data->geometry->GetProperty(kPropertyKiller) != nullptr)
				KillPlayer();
			else if(data->geometry->GetProperty(kPropertyMover) != nullptr) {
				ResetNode();
				model->GetSuperNode()->RemoveSubnode(model);
				data->geometry->GetController()->GetTargetNode()->AddSubnode(GetTargetNode());
				moverPos = model->GetSuperNode()->GetWorldPosition();
				currState = kStateNone;
			}
			else {
				ResetNode();
			}
		}
		HandleCollision(GetCollisionData());
	}

	SetFinalPosition(finalPos);

	//
	switch(currState) {
		/**
		 *  Turning
		 */
		case kStateTurning: {
			// get the goal azm and direction rotating
			float azm = (facingLeft) ? K::pi_over_2 : -K::pi_over_2;
			float dir = (facingLeft) ? 1.f : -1.f;

			// handle turning right
			if(facingLeft) {
				if(modelAzimuth >= azm) {
					modelAzimuth = azm;
					currState = kStateNone;
				} else {
					float dr = K::playerRotationSpeed * TheTimeMgr->GetDeltaTime();
					modelAzimuth += dir * dr;
				}
			}
			// handle turning left
			else {
				if(modelAzimuth <= azm) {
					modelAzimuth = azm;
					currState = kStateNone;
				} else {
					float dr = K::playerRotationSpeed * TheTimeMgr->GetDeltaTime();
					modelAzimuth += dir*dr;
				}
			}
			break;
		}


		/**
		 *  Pivoting
		 */
		case kStatePivoting: {
			// get the goal azm and direction rotating
			int dir = (pivotDir>0) ? 1 : -1;
			dir *= (hasPivoted) ? -1 : 1;

			float azm = startAzm + ((dir>0) ? K::pi_over_2 : -K::pi_over_2);

			// handle turning right
			if(dir>0) {
				if(playerAzimuth >= azm) {
					playerAzimuth = azm;
					currState = kStateNone;
				} else {
					float dr = K::playerPivotSpeed * TheTimeMgr->GetDeltaTime();
					playerAzimuth += dir * dr;
				}
			}
			// handle turning left
			else {
				if(playerAzimuth <= azm) {
					playerAzimuth = azm;
					currState = kStateNone;
				} else {
					float dr = K::playerPivotSpeed * TheTimeMgr->GetDeltaTime();
					playerAzimuth += dir*dr;
				}
			}
			break;
		}

		/**
		 *  Hanging
		 */
		case kStateHanging: {
			// set position to the hang
			model->SetNodePosition(model->GetSuperNode()->GetInverseWorldTransform() * hangPos);
			SetColliderPosition(model->GetSuperNode()->GetInverseWorldTransform() *  hangPos);
			SetFinalPosition(hangPos);
			break;
		}

		case kStateRope: {
			// set position to the hang
			Point3D pos = hangPos - Vector3D(Cos(modelAzimuth+K::pi_over_2)*.2f,Sin(modelAzimuth+K::pi_over_2)*.2f, 0);
			model->SetNodePosition(model->GetSuperNode()->GetInverseWorldTransform() * pos);
			SetColliderPosition(model->GetSuperNode()->GetInverseWorldTransform() *  pos);
			SetFinalPosition(hangPos);
			break;
		}

		/**
		 *  Turning
		 */
		case kStateRopeTurn: {
			// get the goal azm and direction rotating
			float azm = (facingLeft) ? K::pi_over_2 : -K::pi_over_2;
			float dir = (facingLeft) ? 1.f : -1.f;

			// handle turning right
			if(facingLeft) {
				if(modelAzimuth >= azm) {
					modelAzimuth = azm;
					currState = kStateRope;
				} else {
					float dr = K::playerRotationSpeed * TheTimeMgr->GetDeltaTime();
					modelAzimuth += dir * dr;
				}
			}
			// handle turning left
			else {
				if(modelAzimuth <= azm) {
					modelAzimuth = azm;
					currState = kStateRope;
				} else {
					float dr = K::playerRotationSpeed * TheTimeMgr->GetDeltaTime();
					modelAzimuth += dir*dr;
				}
			}
			Point3D pos = hangPos - Vector3D(Cos(modelAzimuth+K::pi_over_2)*.2f,Sin(modelAzimuth+K::pi_over_2)*.2f, 0);
			model->SetNodePosition(model->GetSuperNode()->GetInverseWorldTransform() * pos);
			SetColliderPosition(model->GetSuperNode()->GetInverseWorldTransform() *  pos);
			SetFinalPosition(hangPos);

			break;
		}

		case kStateClimbUp: {
			// set position to the hang
			if(climbTime < 3050) {
				model->SetNodePosition(model->GetSuperNode()->GetInverseWorldTransform() * hangPos);
				SetColliderPosition(model->GetSuperNode()->GetInverseWorldTransform() *  hangPos);
				SetFinalPosition(hangPos);
			}
			else {
				currState = kStateNone;
				climbTime = 0;
				model->SetNodePosition(model->GetSuperNode()->GetInverseWorldTransform() * (lastPos + Point3D(5,5,5)));
				//SetColliderPosition(model->GetSuperNode()->GetInverseWorldTransform() *  (lastPos + Point3D(5,5,5)));

				SetFinalPosition(finalPos + Point3D(Cos(playerAzimuth),Sin(playerAzimuth),3));
				Invalidate();
			}
			break;
		}

		case kStateDead: {
			SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
			side->SetRotationAzm(side->GetRotationAzm() + K::playerRotationSpeed * TheTimeMgr->GetDeltaTime()/20);
			break;
		}

		case kStateRespawn: {
			Spawn();
			ResetNode();
			model->SetNodePosition(model->GetSuperNode()->GetInverseWorldTransform() * GetLastCheckpoint());
			SetFinalPosition(GetLastCheckpoint());
		}
	}

	// rotate the model the modelAzimuth (offset) + playerAzimuth
	model->SetNodeMatrix3D(model->GetSuperNode()->GetInverseWorldTransform() * Matrix3D().SetRotationAboutZ(playerAzimuth-modelAzimuth));
	
	// finally, animate the player
	model->Animate();
}



void PlayerController::SetAnim(PlayerAnim anim)
{
	// This function sets the animation resource corresponding to
	// the current type of motion assigned to the player.
	
	Interpolator *interpolator = frameAnimator->GetFrameInterpolator();
	if (anim == kAnimIdle)	{
		frameAnimator->SetAnimation("Char_Idle");
		interpolator->SetRange(0.f, 2048.0F);
		interpolator->SetMode(kInterpolatorForward | kInterpolatorLoop);
	}
	else if (anim == kAnimWalk)
	{
		frameAnimator->SetAnimation("Char_Walk");
		interpolator->SetRange(0.0F, 1088.f);
		interpolator->SetMode(kInterpolatorForward | kInterpolatorLoop);
	}
	else if (anim == kAnimRun)
	{
		frameAnimator->SetAnimation("Char_Run");
		interpolator->SetRange(0.0F, 800.0F);
		interpolator->SetMode(kInterpolatorForward | kInterpolatorLoop);
	}
	else if (anim == kAnimJump)
	{
		frameAnimator->SetAnimation("Char_Jump");
		interpolator->SetRange(0.f, 1024.f);
		interpolator->SetMode(kInterpolatorForward);
	}
	else if (anim == kAnimJumpUp)
	{
		frameAnimator->SetAnimation("Char_JumpUp");
		interpolator->SetRange(0.f, 1888.f);
		interpolator->SetMode(kInterpolatorForward);
	}
	else if (anim == kAnimFalling)
	{
		frameAnimator->SetAnimation("Char_Jump");
		interpolator->SetRange(640.f, 1024.f);
		interpolator->SetMode(kInterpolatorForward);
	}
	else if (anim == kAnimHanging)
	{
		frameAnimator->SetAnimation("Char_ClimbFromHang");
		interpolator->SetRange(0.f, 736.f);
		interpolator->SetMode(kInterpolatorForward);
	}
	else if (anim == kAnimHangFalling)
	{
		frameAnimator->SetAnimation("Char_JumpUp");
		interpolator->SetRange(960.f, 1888.f);
		interpolator->SetMode(kInterpolatorForward);
	}
	else if (anim == kAnimClimbUp)
	{
		frameAnimator->SetAnimation("Char_ClimbFromHang");
		interpolator->SetRange(736.f, 3680.f);
		interpolator->SetMode(kInterpolatorForward);
	}
	else if (anim == kAnimDead)
	{
		frameAnimator->SetAnimation("Char_Death");
		interpolator->SetRange(0.f, 2048.f);
		interpolator->SetMode(kInterpolatorForward);
	}

	playerAnim = anim;
}