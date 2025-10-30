// Progress bar overlay functionality
            Shiny.addCustomMessageHandler('showProgress', function(message) {
              var overlay = document.getElementById('progress-overlay');
              if (!overlay) {
                overlay = document.createElement('div');
                overlay.id = 'progress-overlay';
                overlay.innerHTML = `
                  <div class='progress-container'>
                    <div class='progress-text'>${message.text || 'Loading...'}</div>
                    <div class='progress-bar-wrapper'>
                      <div class='progress-bar' id='progress-bar'></div>
                    </div>
                  </div>
                `;
                document.body.appendChild(overlay);
              }
              overlay.style.display = 'flex';
              var progressBar = document.getElementById('progress-bar');
              progressBar.style.width = '0%';
              
              // Simulate progress
              var width = 0;
              var interval = setInterval(function() {
                if (width >= 90) {
                  clearInterval(interval);
                } else {
                  width += Math.random() * 10;
                  if (width > 90) width = 90;
                  progressBar.style.width = width + '%';
                }
              }, 200);
            });
            
            Shiny.addCustomMessageHandler('hideProgress', function(message) {
              var overlay = document.getElementById('progress-overlay');
              if (overlay) {
                var progressBar = document.getElementById('progress-bar');
                progressBar.style.width = '100%';
                setTimeout(function() {
                  overlay.style.display = 'none';
                }, 300);
              }
            });
            