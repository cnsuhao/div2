#ifndef Game_h
#define Game_h

// Include C4 libraries
#include "C4Application.h"
#include "C4Configuration.h"
#include "C4Engine.h"
#include "C4ColorPicker.h"
#include "C4List.h"

// Include Game liberaries
#include "GameWorld.h"
#include "Input.h"
#include "Player.h"
#include "Interface.h"


extern "C" { module C4::Application *ConstructApplication(void); }

namespace C4
{	
	// Every application/game module needs to define a subclass of the Application
	// class to serve as the primary interface with the engine. This subclass is
	// constructed and returned to the engine in the ConstructApplication() function.
	// There should be only one instance of this class, so it inherits from the
	// Singleton template. A pointer to the Game instance is declared below.
	
	class Game : public Singleton<Game>, public Application
	{
		private:
			bool							camTriggers;
			bool							gridMove;
			bool							loadWorld;
			bool							showText;

			char*							nextWorld;

			TextElement						*loadingText;

			DisplayEventHandler				displayEventHandler;
			
			EntityRegistration				playerEntityReg;
			LocatorRegistration				locatorReg;
		
			InputMgr::keyProcType			prevEscapeProc;
			void							*prevEscapeData;

			List<GameInterface>				interfaceList;
			List<GameWindow>				windowList;
			
			PlayerController				*playerController;

			bool							playerMonkeys[3];
			long							playerCoinAmount;
						
			static World *ConstructWorld(const char *name, void *data);
			
			static void HandleDisplayEvent(EventType eventType, long param, void *data);
			
			static void EscapeProc(void *data);
			
			void ApplicationTask();
		
		protected:
			// in protected so only Cam tool has access DANGEROUS FUNCTION
			void *SetPlayerController(PlayerController *pc) {
				playerController = pc;
			}

		public:
			Game();
			~Game();
			
			PlayerController *GetPlayerController(void) const {
				return (playerController);
			}	
			//*********** Stuff I just added
			void AddInterface(GameInterface *interface)
			{
				interfaceList.Append(interface);
				TheInterfaceMgr->AddElement(interface);
			}

			void AddWindow(GameWindow *window)
			{
				windowList.Append(window);
				TheInterfaceMgr->AddElement(window);
			}

			WorldResult StartSinglePlayerGame(const char *name);
			void ExitCurrentGame(bool unload = true);

			//******************
			EngineResult LoadWorld(const char *name);
			void UnloadWorld(void);

			void ToggleCamTriggers() { camTriggers = !camTriggers; }
			bool GetCamTriggers() { return camTriggers; }

			void SetWorldLoad(bool loadFlag){ loadWorld = loadFlag; }
			void SetNextWorld(char* worldName){ nextWorld = worldName; }

			void ToggleGridMove() { gridMove = !gridMove; }
			bool GetGridMove() { return gridMove; }

			Point3D GetCheckpoint(void) {
				return Point3D(0,0,0);
			}
	};
	
	
	// This is a pointer to the one instance of the Game class through which
	// any other part of the application/game module can access it.
	extern Game *TheGame;
}

#endif
