playlist <- c("song1.m4a", "song2.m4a", "song3.m4a")


ui <- fluidPage(

  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "intalcade.css")),

  tags$head(
    tags$script(HTML(sprintf("
      let inactivityTimeout;
      const inactivityLimit = 60000; // 1 minute
      const playlist = %s;
      let currentIndex = 0;
      let audioPlayer;
      let onHome = true;
      let idleMode = false;  // tracks whether we're in the idle / media mode

      function resetInactivityTimer() {
        clearTimeout(inactivityTimeout);
        // If we are in idle mode, ignore reset?
        inactivityTimeout = setTimeout(function() {
          if (onHome && !idleMode) {
            Shiny.setInputValue('user_inactive', new Date().getTime());
          }
        }, inactivityLimit);
      }

      function playNext() {
        if (currentIndex >= playlist.length) return;
        const audioSrc = playlist[currentIndex];
        if (audioPlayer) {
          audioPlayer.remove();
        }
        audioPlayer = document.createElement('audio');
        audioPlayer.src = audioSrc;
        audioPlayer.autoplay = true;
        audioPlayer.type = 'audio/mp4';
        audioPlayer.addEventListener('ended', function() {
          currentIndex++;
          playNext();
        });
        document.body.appendChild(audioPlayer);
      }

      function startPlaylist() {
        if (!playlist.length) return;
        idleMode = true;
        currentIndex = 0;
        playNext();
      }

      function wakeUp() {
        if (idleMode) {
          idleMode = false;
          // tell Shiny to go home
          Shiny.setInputValue('wake_up', new Date().getTime());
          // stop any audio
          if (audioPlayer) {
            audioPlayer.pause();
            audioPlayer.remove();
            audioPlayer = null;
          }
          // also remove GIF etc in Shiny side
        }
      }

      // mousemove and keydown also trigger wake up if idle
      document.onmousemove = function(e) {
        resetInactivityTimer();
        wakeUp();
      };
      document.onkeydown = function(e) {
        resetInactivityTimer();
        wakeUp();
      };
      document.onload = resetInactivityTimer;

      Shiny.addCustomMessageHandler('screen_state', function(message) {
        onHome = (message.state === 'home');
        if (!onHome) {
          // leaving home => stop audio, exit idle
          idleMode = false;
          if (audioPlayer) {
            audioPlayer.pause();
            audioPlayer.remove();
            audioPlayer = null;
          }
        }
      });

      Shiny.addCustomMessageHandler('start_media', function(message) {
        if (onHome && !idleMode) {
          startPlaylist();
        }
      });
    ", jsonlite::toJSON(playlist))))
  ),

  uiOutput("app_page"),
)