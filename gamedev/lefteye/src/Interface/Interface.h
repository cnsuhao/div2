#ifndef Interface_h
#define Interface_h


#include "C4ColorPicker.h"



namespace C4
{
	class PlayerSettingsWindow;
	class GraphicsSettingsWindow;
	class AudioSettingsWindow;
	class ControlSettingsWindow;
	class FilePicker;
	
		
	
	class GameInterface : public Interface, public ListElement<GameInterface>
	{
		protected:
			
			GameInterface();
		
		public:
			
			virtual ~GameInterface();
	};
	
	
	class GameWindow : public Window, public ListElement<GameWindow>
	{
		protected:
			
			GameWindow(float width, float height, const char *title = nullptr, unsigned long flags = kWindowTitleBar | kWindowCloseButton);
		
		public:
			
			virtual ~GameWindow();
	};

	class TextButtonElement : public TextElement
	{
		private:
			
			ColorRGBA		textColor;
			ColorRGBA		hiliteColor;
			
			bool CalculatePickBox(Box2D *box) const;
		
		public:
			
			TextButtonElement(const char *text = nullptr, Font *font = nullptr);
			~TextButtonElement();
			
			void SetColors(const ColorRGBA& text, const ColorRGBA& hilite);
			
			PartCode TestPickPoint(const Point3D& p) const;
			void HandleMouseEvent(EventType eventType, PartCode code, const Point3D& p);
			void Build(void);
	};
	
	
	class DisplayInterface : public GameInterface, public Singleton<DisplayInterface>
	{
		private:
			
			ImageElement	coinImage;
			ImageElement	monkeyEmptyImage;
			ImageElement	monkeyHeadImage;
			ImageElement	monkeyBodyImage;
			ImageElement	monkeyFeetImage;
			
			TextElement		coinValueText;
			
			TextElement		monkeyText;//text describing monkey
			TextElement		monkeyValueText; //number of pieces
						
			
			QuadElement		ammoQuad[4];
		
		public:
			
			DisplayInterface();
			~DisplayInterface();
			
			static void Open(void);
			static void UpdateAll(void);
						
			void UpdateDisplayPosition(void);
			void UpdatePlayerCoin(void);
			void UpdatePlayerMonkey(void);
			
			

	};
	

	
	
	class StartWindow : public GameWindow, public Completable<StartWindow>
	{
		protected:
			
			PushButtonElement		*cancelButton;
			ListBoxElement			*worldListBox;
			ImageElement			*previewImage;
			
			StringTable				*worldTable;
			long					worldSelection;
			
			StartWindow();
			
			void UpdatePreview(void);
		
		public:
			
			~StartWindow();
			
			const char *GetSelectedWorldName(void) const;
			
			bool HandleKeyboardEvent(EventType eventType, long key, unsigned long modifiers);
			void HandleElementTrigger(Element *element);
	};
	
	
	class SinglePlayerWindow : public StartWindow, public Singleton<SinglePlayerWindow>
	{
		private:
			
			PushButtonElement		*startButton;
			
			SinglePlayerWindow();
			
		public:
			
			~SinglePlayerWindow();
			
			static void Open(void);
			
			void InterfaceTask(void);
			bool HandleKeyboardEvent(EventType eventType, long key, unsigned long modifiers);
			void HandleElementTrigger(Element *element);
	};
		
	class MainWindow : public GameWindow, public Singleton<MainWindow>
	{
		private:
			
			TextButtonElement		*startNewGameButton;
			TextButtonElement		*loadSavedGameButton;
			TextButtonElement		*saveCurrentGameButton;
			
			TextButtonElement		*hostGameButton;
			TextButtonElement		*joinGameButton;
			
			TextButtonElement		*playerSettingsButton;
			TextButtonElement		*controlSettingsButton;
			TextButtonElement		*graphicsSettingsButton;
			TextButtonElement		*audioSettingsButton;
			
			TextButtonElement		*exitCurrentGameButton;
			TextButtonElement		*quitButton;
			
			MainWindow();
			
			static void SinglePlayerComplete(StartWindow *window, void *data);
		
		public:
			
			~MainWindow();
			
			static void Open(void);
			
			bool HandleKeyboardEvent(EventType eventType, long key, unsigned long modifiers);
			void HandleElementTrigger(Element *element);
	};
	
	




	extern DisplayInterface *TheDisplayInterface;

	extern SinglePlayerWindow *TheSinglePlayerWindow;



	extern MainWindow *TheMainWindow;
}


#endif
