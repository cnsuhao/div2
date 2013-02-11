#ifndef KillMover_h
#define KillMover_h

#include "C4Configuration.h"
#include "C4Collision.h"
#include "C4Sources.h"

/**This is a basic trap that kills the player when stepped on.
*
*/
namespace C4
{
	class Variable;

	/// This is the type for the controller that we use
	enum
	{
		kControllerKillMover = 'kmov'
	};

	enum
	{
		kColliderKillMover = kColliderBaseClass
	};
	
	enum {
		kAxisX  = 'xaxi',
		kAxisY  = 'yaxi',
		kAxisZ = 'zaxi',
		kAxisXneg = 'xneg',
		kAxisYneg = 'yneg',
		kAxisZneg = 'zneg'
	};

	/// A controller for kill trap
	class KillMoverController : public SphereCollider //Controller
	{
	private:
		Point3D		originalPosition;
		Point3D		position;
		Vector3D	velocity;
	

		long		travelAxis;
		
		float		offset;
		float		speed;
		float		travelDistance;

		bool		reverseDirection; 
		bool		doesPatrol;
		bool		repeatMovement;
		bool		stopMoving;
		bool		hasTrigger;
		bool		killPlayer;

		KillMoverController(const KillMoverController& killMoverController);
		

        Controller *Replicate(void) const;

	public:
		KillMoverController();
		KillMoverController(float newSpeed);
		~KillMoverController();

		long GetSettingCount(void) const;
        Setting *GetSetting(long index) const;
		void SetSetting(const Setting *setting);

		// Serialization functions
		unsigned long GetPackSize(unsigned long packFlags) const;
		void Pack(Packer& data, unsigned long packFlags) const;
		void Unpack(Unpacker& data, unsigned long unpackFlags);

		Point3D CalculatePosition(long axis, float newOffset, bool reverse);

		void Preprocess();
		void Move();
		void Travel();
	};
}


#endif