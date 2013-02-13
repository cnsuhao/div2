#ifndef AdvCameraScripts_h
#define AdvCameraScripts_h

#include "C4Scripts.h"
#include "Cameras.h"

namespace C4
{
	enum {
		kMethodCamBoxTransition	= 'cbox'
	};

	// axis enum
	enum {
		kx = 'cmkx',
		ky = 'cmky',
		kz = 'cmkz'
	};

	enum {
		kbDist = 1 << 0,
		kbAzm  = 1 << 1,
		kbAlt  = 1 << 2,
		kbXY   = 1 << 3,
		kbZ    = 1 << 4
	};

	class CamBoxTransitionMethod : public Method
	{
		friend class MethodReg<CamBoxTransitionMethod>;
		private:
			float	scale;
			long	axis;

			float	strtDist;
			float	strtAzm;
			float	strtAlt;
			float	strtXY;
			float	strtZ;

			float	goalDist;
			float	goalAzm;
			float	goalAlt;
			float	goalXY;
			float	goalZ;

			long	boolFlags;

			CamBoxTransitionMethod();
			CamBoxTransitionMethod(const CamBoxTransitionMethod& camBoxTransitionMethod);
			
			Method *Replicate(void) const;
		
			bool UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags);
		public:
			CamBoxTransitionMethod(float s, long ax, float sd, float saz, float sal, float sxy, float sz, float gd, float gaz, float gal, float gxy, float gz, long bf);
			~CamBoxTransitionMethod();

			float GetScale(void) const { return (scale); }
			void  SetScale(float s) { scale = s; }
			long  GetAxis(void) const { return axis; }
			void  SetAxis(long a) { axis = a; }

			float GetStrtDist(void) const { return (strtDist); }
			void  SetStrtDist(float d) { strtDist = d; }
			float GetStrtAzm(void) const { return (strtAzm); }
			void  SetStrtAzm(float a) { strtAzm = a; }
			float GetStrtAlt(void) const { return (strtAlt); }
			void  SetStrtAlt(float a) { strtAlt = a; }
			float GetStrtXY(void) const { return (strtXY); }
			void  SetStrtXY(float xy) { strtXY = xy; }
			float GetStrtZ(void) const { return (strtZ); }
			void  SetStrtZ(float z) { strtZ = z; }

			float GetGoalDist(void) const { return (goalDist); }
			void  SetGoalDist(float d) { goalDist = d; }
			float GetGoalAzm(void) const { return (goalAzm); }
			void  SetGoalAzm(float a) { goalAzm = a; }
			float GetGoalAlt(void) const { return (goalAlt); }
			void  SetGoalAlt(float a) { goalAlt = a; }
			float GetGoalXY(void) const { return (goalXY); }
			void  SetGoalXY(float xy) { goalXY = xy; }
			float GetGoalZ(void) const { return (goalZ); }
			void  SetGoalZ(float z) { goalZ = z; }

			long GetBoolFlags(void) const { return boolFlags; }
			void SetBoolFlags(long f) { boolFlags = f; } 
			
			unsigned long GetPackSize(unsigned long packFlags) const;
			void Pack(Packer& data, unsigned long packFlags) const;
			void Unpack(Unpacker& data, unsigned long unpackFlags);

			long GetSettingCount(void) const;
			Setting *GetSetting(long index) const;
			void SetSetting(const Setting *setting);
			
			void Execute(const ScriptState *state);
	};
}

#endif