#include "Cameras.h"

#include "C4World.h"
#include "Game.h"

using namespace C4;

/// Side scroller Camera
SideCamera::SideCamera() : FrustumCamera(2.0F, 1.0F) {}
SideCamera::~SideCamera() {}

void SideCamera::Move(void)
{
	if (targetEntity)
	{
		// entity position
		const Point3D& position = targetEntity->GetWorldPosition();

		// reset camera height if camera is locked
		if(trackType == kCamLock) {
			ResetHeight();
		}
		
		// get the orientation of the camera (assume entity is at 0,0,0)
		PlayerController *controller = static_cast<PlayerController *>(targetEntity->GetController());
		float azm = controller->GetPlayerAzimuth();

		controller->GetModelAzimuth();

		// compensate for flipping
		float rAzm = ((flipFlags & kFlipAzm) == 0) ? rotationAzm : rotationAzm * Cos(controller->GetModelAzimuth()+K::pi_over_2);
		float offXY = ((flipFlags & kFlipXY) == 0) ? offset.x : offset.x * Cos(controller->GetModelAzimuth()+K::pi_over_2);

		// other variables set
		float rAlt = rotationAlt;
		float offZ = offset.y;

		// calculate new azm 
		float dAzm = azm + rAzm;

		/** 
		 * Calculate new position 
		 */
		// base position
		Point3D cameraRoot(position.x, position.y, camHeight);

		// calculate position shift due to Azm and Alt
		Vector3D posShift(dist,0.f,0.f);
		posShift = posShift.RotateAboutZ(dAzm+K::pi_over_2);
		posShift = posShift.RotateAboutX(-rAlt*Cos(dAzm));
		posShift = posShift.RotateAboutY(-rAlt*Sin(dAzm));
		
		// calculate the offset coords
		Vector2D xyOffset(offXY * Sin(dAzm+K::pi_over_2), -offXY * Cos(dAzm+K::pi_over_2));
		Point3D posOffset(xyOffset.x, xyOffset.y, offZ);

		// Final Position
		Point3D  finalPos = cameraRoot-posShift+posOffset;

		/** 
		 * Calculate new rotation (stolen from chase cam)
		 */
		azm = dAzm + K::pi_over_2;

		float alt;
		if(trackType == kCamTrack) {		
			// calculate the shift from root in XY and change in z
			float dXY = Sqrt(Pow(posShift.x, 2.f) + Pow(posShift.y, 2.f));
			float dZ  = -posShift.z;

			// calculate the distance of the player from the camHeight
			float dH = (camHeight - position.z)*trackAmount;

			// calculate the alt
			alt = Atan(dZ+dH, dXY);		
		} else {
			// rotation without track
			alt = rAlt;
		}
		
		float cp = Cos(-alt);
		float sp = Sin(-alt);
		
		float ct = Cos(azm);
		float st = Sin(azm);
		
		Vector3D view(ct*cp, st*cp, sp);
		Vector3D right(st, -ct, 0.0F);
		Vector3D down = view % right;	

		// Move Camera
		SetNodeTransform(right,down,view,finalPos);
	}
}


/////////////////  Set Camera Attributes

/**
 *	ResetHeight sets height the Z of the target entity
 */
void SideCamera::ResetHeight() {
	if (targetEntity) {
		const Point3D& position = targetEntity->GetWorldPosition();
		camHeight = position.z;
	}
}

/**
 *	Set the offset vector (x is multiple in x/y and y is offset in z)
 *  @param v vector to copy
 */
void SideCamera::SetOffset(Vector2D v) {
	offset = v;
}

void SideCamera::SetOffsetXY(float xy) {
	offset = Vector2D(xy,offset.y);
}

void SideCamera::SetOffsetZ(float z) {
	offset = Vector2D(offset.x,z);
}

/**
 *	Set the offset distance
 *  @param d distance to player entity
 */
void SideCamera::SetDist(float d) {
	dist = d;
}

/**
 *	Set the rotation around the player as if the player were in the center of a unit circle
 *  @param azm yaw
 *  @param alt pitch
 */
void SideCamera::SetRotation(float azm, float alt) {
	rotationAzm = azm;
	rotationAlt = alt;
}

void SideCamera::SetRotationAzm(float azm) {
	rotationAzm = azm;
}

void SideCamera::SetRotationAlt(float alt) {
	rotationAlt = alt;
}

/**
 *	Set the type of tracking to lock the camera to the player
 *  @param type based on enum
 */
void SideCamera::SetTrackType(long type) {
	trackType = type;
}

/*	Set the percentage tracked
 *  @param alt pitch
 */
void SideCamera::SetTrackAmount(float amt) {
	trackAmount = amt;
}