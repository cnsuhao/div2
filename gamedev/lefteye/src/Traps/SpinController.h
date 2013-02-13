#ifndef SpinController_h
#define SpinController_h

#include "C4Properties.h"
#include "C4Configuration.h"
#include "C4Sources.h"
#include "C4Controller.h"


namespace C4
{

	// These are the controller-specific message IDs
	enum
	{
		kControllerMessageChangeRotation	= 0,
		kControllerMessageSetRotationState	= 1,
	};


	// Controller types
	enum
	{
		kControllerSpin			= 'spin'
	};

class SpinController : public Controller
{
   private:

        float         spinRate;             // In radians per millisecond
        float         spinAngle;            // The current angle, in radians
		float		  startAngle;
		float		  endAngle;

		long		rotationAxis;

		bool		  reverseDirection;
		bool		  shouldSwing;

        Transform4D   originalTransform;    // The target's original transform

        SpinController(const SpinController& spinController);

        Controller *Replicate(void) const;

   public:

        SpinController();
        SpinController(float rate);
        ~SpinController();

        float GetSpinRate(void) const
        {
            return (spinRate);
        }

        void SetSpinRate(float rate)
        {
            spinRate = rate;
        }

        
        // Serialization functions
        unsigned long GetPackSize(unsigned long packFlags) const;
        void Pack(Packer& data, unsigned long packFlags) const;
        void Unpack(Unpacker& data, unsigned long unpackFlags);

        // User interface functions
        long GetSettingCount(void) const;
        Setting *GetSetting(long index) const;
        void SetSetting(const Setting *setting);

        void Preprocess(void);

        // The function that moves the target node
        void Move(void);
};
}
#endif