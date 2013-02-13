#ifndef Cameras_h
#define Cameras_h

#include "C4Cameras.h"

namespace C4
{
	// different tracking types
	enum {
		kCamNone  = 'none',
		kCamLock  = 'lock',
		kCamTrack = 'trck'
	};

	enum {
		kFlipXY  = 1 << 0,
		kFlipAzm = 1 << 1
	};


	// class for the sidescroller camera
	class SideCamera : public FrustumCamera
	{
		private:
			
			Entity		*targetEntity;

			// height of camera
			float camHeight;
			float dist;

			long trackType;
			float trackAmount;

			float rotationAzm;
			float rotationAlt;

			unsigned long flipFlags;

			Vector2D offset;

		public:
			SideCamera();
			~SideCamera();
			
			Entity *GetTargetEntity(void) const	{
				return (targetEntity);
			}
			
			void SetTargetEntity(Entity *entity) {
				targetEntity = entity;
			}

			void Move(void);
			void ResetHeight(void);
			void SetOffset(Vector2D v);
			void SetOffsetXY(float xy);
			void SetOffsetZ(float z);
			void SetDist(float d);
			void SetRotation(float azm, float alt);
			void SetRotationAzm(float azm);
			void SetRotationAlt(float alt);
			void SetTrackType(long type);
			void SetTrackAmount(float amt);

			void SetFlipFlags(unsigned long ff) { flipFlags = ff; }

			float GetDist() { return dist; }
			float GetOffsetXY() { return offset.x; }
			float GetOffsetZ() { return offset.y; }
			float GetRotationAzm() { return rotationAzm; }
			float GetRotationAlt() { return rotationAlt; }

			unsigned long GetFlipFlags() { return flipFlags; }
	};
}


#endif
