# Laura XV Anos - Website

This is the repository for the Laura XV Anos website, developed for Laura's 15th birthday celebration. The website provides information about the event, including a photo gallery, gift list, comments section, and RSVP form. It was designed with a focus on delivering a modern and elegant visual experience.

### Website URL:

[lauraxv.fly.dev
](https://lauraxv.fly.dev/)



### Technologies Used
        
    Frontend:
        HTML5
        Tailwind CSS
        Lustre: Gleam Frontend Framework

    Backend:
        Gleam: functional language used for server-side logic and database integration.
        SQLite: lightweight database for storing RSVP confirmations and other data.

    Tools and Infrastructure:
        Docker: used to create and manage the development environment.
        Fly.io: platform for deployment and hosting.

### Features
Interactive Navigation: The navigation bar is configured to display only the selected tab’s name, with side arrows to switch between tabs.
    RSVP Confirmation: A form for guests to confirm attendance and add companions.
    Gift List: Displays a list of gifts guests can choose to buy.
    Photo Gallery: Gallery displaying photos of the event.
    Comments Section: An area where guests can leave messages for Laura.

Installation and Running Locally
Prerequisites:

    Docker installed
    Gleam

Steps to Run Locally:

    Clone the repository:

    git clone https://github.com/raphaelantoniocampos/lauraxv
    
Set the .env file inside server subproject:

    DATABASE_PATH=" ../db/database.sqlite3"
    SECRET_KEY_BASE="64x characters"
    GLEAM_ENV="development"
    PORT="8000"

Run the project

    cd lauraxv
    sh run-dev.sh

The site will be available at http://localhost:8080.

### Project Structure

/client              
Frontend subproject (Lustre)

/client/priv         
Images and other static files

/server              
Backend logic written in Gleam

/db                  
Sqlite database file



### Contribution

Contributions are welcome! To suggest improvements or new features, please open an issue or submit a pull request.
