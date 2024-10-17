# Meeting Pierre 28.04.2023

- Braucht es eigentlich TPAToken? --> komplett entfernen? --> Ja!
- Ich lass das secret jetzt drin. Das ELN sendet dann das secret an das ELN zur verification --> erledigt
- secret komplett entfernen --> erledigt

- Zuweisung von Fileextensions zu TPAs
	+ Dropdown nur für richtige Apps
	+ Im Admin Board

- https://chemotion.net/docs/development/linting
	+ RuboCop laufen lassen --> erledigt
	+ ESLint laufen lassen --> erledigt? Der hat nichts angemerkt!

- AdminDashboard --> erledigt
	+ Test ob User wirklich Admin ist im Backend! --> erledigt
		* siehe chatGPT

- remove method getIPAddress --> und mach das in third_party_app_api.rb --> erledigt

- möglichst wenig code

- Tests schreiben für alles
	+ siehe chatGPT

- Tests:
	+ https://chemotion.net/docs/development/testing
	+ ruby tests am wichtigsten
	+ unit tests for backend
	+ entities & api



# To Do

- check ob Admin Oberfläche läuft
	+ erstellen funktioniert
	+ delete funktioniert --> black screen im hintergrund warum? Zufall?
	+ editieren funktioniert --> aber man muss den Namen ändern, da der ansonsten nicht unique ist. Muss gefixt werden
- add check if user is admin
	+ new, delete und update funktioniert. --> erledigt
- check ob ResearchPlan Oberfläche läuft
	+ ja tut sie. Man wird zur App weitergeleitet
- entfernen von TPAtoken im Frontend --> erledigt
- entfernen von TPAtoken im Backend --> erledigt
- entfernen von secret --> erledigt

- anpassen der public api --> erledigt
- in comeln download und upload anpassen --> erledigt

- Tests schreiben

- MTT test in Biostats neu schreiben/integrieren
- Plot bessere Errors
- Nicht das ganze file zeigen als notification wenn upload in R fehlschlägt --> erledigt



# Tests

- Wenn man config.web_console.permissions = '172.18.0.1' zu config/environments/development.rb hinzufügt und es von config/application.rb entfernt
	+ kann man die tests mit RAILS_ENV=test bundle exec rspec spec/models/third_party_app_spec.rb im dockercontainer direkt im terminal laufen lassen
	+ aber die permission für den get request funktioniert immer noch nicht
			- --> allerdings unauthorzed error 401, davor war es permission denied 403
				+ macht nix wenn man dann in die rails console geht wird es wieder 403
	+ das heißt ich würde jetzt weiter in der rails console testen
		* dort muss dann das permission problem gelöst werden
- bundle install im terminal auf meinem PC --> Idee tests auf PC laufen lassens
	+ sudo apt-get install ruby-dev --> für raccc
	+ sudo apt-get install swig --> für inchi-gem
	+ manchmal muss sudo bundle install gelaufen lassen werden --> wenn nicht nach sudo password gefragt wird
	+ run bundle install or sudo bundle install
	+ sudo gem install rdkit_gem
	+ RAILS_ENV=test bundle exec rspec spec/models/third_party_app_spec.rb
		* LoadError:
  cannot load such file -- /var/lib/gems/3.0.0/bundler/gems/rdkit_chem-cf8a3890c5aa/rdkit_chem/lib/RDKitChem
  - oder doch docker erlauben auf ip zuzugreifen?



# Meeting Fabian 24.05.2023

  - zu application.rb: config.web_console.development_only = false
  - zu group development, test & test: gem 'web-console'
  - im Docker container:
  	- bundle exec rspec spec/api/chemotion/sample_api_spec.rb:680
  	- RAILS_ENV=test bundle exec rspec spec/models/third_party_app_spec.rb
  - run test in docker container
  	+ immer noch unallowed 401


# Meeting Pierre 02.06.2023

 - Doku schreiben
 - in google docs
 - erklären was erwartet man
 	+ aus User sicht
 	+ aus Admin sicht
 - erklären was dann bei den einzelenen Schritten im Hintergrund passiert
 	+ z.B. token wird geholt etc.
 - Damit verkauft man die THirdPartyApp
 - 1-2 Seiten
