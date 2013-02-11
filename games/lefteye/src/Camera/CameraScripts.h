#ifndef CarmeraScripts_h
#define CarmeraScripts_h

#include "C4Scripts.h"
#include "Cameras.h"

namespace C4
{
	enum
	{
		kMethodCamChangeType	= 'ctyp',
		kMethodCamReset			= 'cres',
		kMethodCamDist			= 'cdst',
		kMethodCamTrackAmt		= 'ctka',
		kMethodCamOffset		= 'coff',
		kMethodCamRotation		= 'crot',
		kMethodCamFlagFlip		= 'cflp',
		kMethodPlayAzm			= 'pazm'
	};

	///////   No Vars
	// reset the camera
	class ResetCameraMethod : public Method	{
		private:
			ResetCameraMethod(const ResetCameraMethod& resetCameraMethod);
			Method *Replicate(void) const;
		
		public:
			ResetCameraMethod();
			~ResetCameraMethod();
			void Execute(const ScriptState *state);
	};


	///////   One Var
	// set the camera distance
	class CamDistMethod : public Method
	{
		friend class MethodReg<CamDistMethod>;
		private:
			float dist;
			
			CamDistMethod();
			CamDistMethod(const CamDistMethod& camDistMethod);
			Method *Replicate(void) const;
		
		public:
			CamDistMethod(float d);
			~CamDistMethod();
			
			float GetDist(void) const {
				return dist;
			}
			
			void SetDist(float d) {
				dist = d;
			}
			
			unsigned long GetPackSize(unsigned long packFlags) const;
			void Pack(Packer& data, unsigned long packFlags) const;
			void Unpack(Unpacker& data, unsigned long unpackFlags);
			
			long GetSettingCount(void) const;
			Setting *GetSetting(long index) const;
			void SetSetting(const Setting *setting);
			
			void Execute(const ScriptState *state);
	};


	// set the Track Amount distance
	class CamTrackAmountMethod : public Method
	{
		friend class MethodReg<CamTrackAmountMethod>;
		private:
			float amt;
			
			CamTrackAmountMethod();
			CamTrackAmountMethod(const CamTrackAmountMethod& camTrackAmountMethod);
			Method *Replicate(void) const;
		
		public:
			CamTrackAmountMethod(float a);
			~CamTrackAmountMethod();
			
			float GetAmt(void) const {
				return amt;
			}
			
			void SetAmt(float a) {
				amt = a;
			}
			
			unsigned long GetPackSize(unsigned long packFlags) const;
			void Pack(Packer& data, unsigned long packFlags) const;
			void Unpack(Unpacker& data, unsigned long unpackFlags);
			
			long GetSettingCount(void) const;
			Setting *GetSetting(long index) const;
			void SetSetting(const Setting *setting);
			
			void Execute(const ScriptState *state);
	};



	///////   Two Var

	// set cam offset
	class CamOffsetMethod : public Method
	{
		friend class MethodReg<CamOffsetMethod>;
		private:
			float xy;
			float z;
			
			CamOffsetMethod();
			CamOffsetMethod(const CamOffsetMethod& camOffsetMethod);
			Method *Replicate(void) const;

			bool UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags);
		
		public:
			CamOffsetMethod(float xy, float z);
			~CamOffsetMethod();
			
			float GetXY(void) const {
				return xy;
			}
			
			void SetXY(float xy) {
				xy = xy;
			}

			float GetZ(void) const {
				return z;
			}
			
			void SetZ(float z) {
				z = z;
			}
			
			unsigned long GetPackSize(unsigned long packFlags) const;
			void Pack(Packer& data, unsigned long packFlags) const;
			void Unpack(Unpacker& data, unsigned long unpackFlags);
			
			long GetSettingCount(void) const;
			Setting *GetSetting(long index) const;
			void SetSetting(const Setting *setting);
			
			void Execute(const ScriptState *state);
	};


	// set camera rotation
	class CamRotationMethod : public Method
	{
		friend class MethodReg<CamRotationMethod>;
		private:
			float azm;
			float alt;
			
			CamRotationMethod();
			CamRotationMethod(const CamRotationMethod& camRotationMethod);
			Method *Replicate(void) const;

			bool UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags);
		
		public:
			CamRotationMethod(float x, float y);
			~CamRotationMethod();
			
			float GetAzm(void) const {
				return azm;
			}
			
			void SetAzm(float xy) {
				azm = azm;
			}

			float GetAlt(void) const {
				return alt;
			}
			
			void SetAlt(float alt) {
				alt = alt;
			}
			
			unsigned long GetPackSize(unsigned long packFlags) const;
			void Pack(Packer& data, unsigned long packFlags) const;
			void Unpack(Unpacker& data, unsigned long unpackFlags);
			
			long GetSettingCount(void) const;
			Setting *GetSetting(long index) const;
			void SetSetting(const Setting *setting);
			
			void Execute(const ScriptState *state);
	};

	///////   Bools
	// what variables flip when player turns around
	class CamFlagFlipMethod : public Method
	{
		friend class MethodReg<CamFlagFlipMethod>;
		private:
			unsigned long flipFlags;
			
			CamFlagFlipMethod();
			CamFlagFlipMethod(const CamFlagFlipMethod& camFlipMethod);
			Method *Replicate(void) const;

		public:
			CamFlagFlipMethod(unsigned long f);
			~CamFlagFlipMethod();
			
			unsigned long GetFlipFlags(void) const {
				return flipFlags;
			}
			
			void SetFlipFlags(unsigned long flags) {
				flipFlags = flags;
			}
			
			unsigned long GetPackSize(unsigned long packFlags) const;
			void Pack(Packer& data, unsigned long packFlags) const;
			void Unpack(Unpacker& data, unsigned long unpackFlags);
			
			long GetSettingCount(void) const;
			Setting *GetSetting(long index) const;
			void SetSetting(const Setting *setting);
			
			void Execute(const ScriptState *state);
	};


	///////   Table
	// change type of camera used
	class CamChangeTypeMethod : public Method
	{
		friend class MethodReg<CamChangeTypeMethod>;
		private:
			long		cameraType;

			CamChangeTypeMethod();
			CamChangeTypeMethod(const CamChangeTypeMethod& CamChangeTypeMethod);
			
			Method *Replicate(void) const;
		
			bool UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags);
		public:
			CamChangeTypeMethod(long type);
			~CamChangeTypeMethod();

			long GetCameraType(void) const {
				return (cameraType);
			}
			
			void SetCameraType(long type) {
				cameraType = type;
			}
			
			unsigned long GetPackSize(unsigned long packFlags) const;
			void Pack(Packer& data, unsigned long packFlags) const;
			void Unpack(Unpacker& data, unsigned long unpackFlags);

			long GetSettingCount(void) const;
			Setting *GetSetting(long index) const;
			void SetSetting(const Setting *setting);
			
			void Execute(const ScriptState *state);
	};




	// set the camera distance
	class PlayerAzmMethod : public Method
	{
		friend class MethodReg<PlayerAzmMethod>;
		private:
			float azm;
			
			PlayerAzmMethod();
			PlayerAzmMethod(const PlayerAzmMethod& playerAzmMethod);
			Method *Replicate(void) const;
		
		public:
			PlayerAzmMethod(float d);
			~PlayerAzmMethod();
			
			float GetAzm(void) const {
				return azm;
			}
			
			void SetAzm(float a) {
				azm = a;
			}
			
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