#ifndef Coin_h
#define Coin_h


#include "C4Controller.h"
#include "C4Triggers.h"
#include "C4Properties.h"
#include "C4Configuration.h"
#include "C4Sources.h"
#include "C4Scripts.h"


namespace C4
{
	
	class Variable;

	/// This is the type for the controller that we use
	enum
	{
		kMethodCollectCoin = 'coin'
	};
	
	class CollectCoinMethod : public Method	{
		private:
			long		amount;
			
			CollectCoinMethod(const CollectCoinMethod& collectCoinMethod);
			Method *Replicate(void) const;
			
			
		
		public:
			CollectCoinMethod();
			~CollectCoinMethod();
			void Execute(const ScriptState *state);

			// Serialization functions
			unsigned long GetPackSize(unsigned long packFlags) const;
			void Pack(Packer& data, unsigned long packFlags) const;
			void Unpack(Unpacker& data, unsigned long unpackFlags);

	        // User interface functions
			long GetSettingCount(void) const;
			Setting *GetSetting(long index) const;
			void SetSetting(const Setting *setting);


	};
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	/*class Variable;

	/// This is the type for the controller that we use for a bunch of Coin
	enum
	{
		kControllerCoin = 'coin'
	};
	

	/// A controller for a bunch of Coin
	class CoinController : public Controller
	{
	private:
		long amount; ///< the amount the Coin is worth

		Trigger* trigger; ///< the trigger that is activated when a player gets near the Coin

		CoinController(const CoinController& coinController);

        Controller *Replicate(void) const;
		
	public:
		CoinController();
		CoinController(long coin);
		~CoinController();

     // Serialization functions
        unsigned long GetPackSize(unsigned long packFlags) const;
        void Pack(Packer& data, unsigned long packFlags) const;
        void Unpack(Unpacker& data, unsigned long unpackFlags);

		void Preprocess();
		void Activate(Node *trigger, Node *activator);
		
	};*/
}


#endif

