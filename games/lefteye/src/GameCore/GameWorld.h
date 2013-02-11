#ifndef GameWorld_h
#define GameWorld_h

#include "C4World.h"

#include "Cameras.h"

namespace C4 {
	// Entity types are associated with a model resource using the EntityRegistration
	// class. Entities are registered with the engine in the Game constructor.
	enum
	{
		kEntityPlayer			= 'sold'
	};
	
	// New locator types are registered with the engine in the Game constructor.
	// The 'spwn' locator is used to specify where the player should be positioned
	// when a world is loaded.
	enum {
		kLocatorSpawn			= 'spwn'
	};

	// The application/game module will usually define a subclass of the World
	// class so that extra information can be associated with the current world.
	// In this case, a pointer to a spawn locator and an instance of the ChaseCamera
	// class is included with the world. A new instance of this World subclass should
	// be returned when the Game::ConstructWorld() function is called (see below).
	class GameWorld : public World
	{
		private:
			
			const LocatorMarker		*spawnLocator;
			SideCamera				sideCamera;
			
			static const LocatorMarker *FindSpawnLocator(const Zone *zone);
		
		public:
			
			GameWorld(const char *name);
			~GameWorld();
			
			const LocatorMarker *GetSpawnLocator(void) const
			{
				return (spawnLocator);
			}
			
			SideCamera *GetSideCamera(void)
			{
				return (&sideCamera);
			}
			
			ResourceResult Preprocess(void);
			void Render(void);
	};
}

#endif
