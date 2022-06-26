/*pirma karta paleidziant programa mes klaida, taciau programa vis tiek veiks(klaida metama nes is 
pradziu bandoma sunaikinti isorini rakta lentelese, kurios dar nera sukurtos)*/
proc sql;
ALTER TABLE zaidejai
	DROP FOREIGN KEY i_komanda
;quit;
/*pirma karta paleidziant programa mes klaida, taciau programa vis tiek veiks(klaida metama nes is 
pradziu bandoma sunaikinti isorini rakta lentelese, kurios dar nera sukurtos)*/
proc sql;
ALTER TABLE Treneriai
	DROP FOREIGN KEY i_treneriai
;quit;


proc sql;
CREATE TABLE Turnyras(
    vieta INT label="Vieta" NOT NULL check (vieta between 1 and 18),
    komanda CHAR(15) NOT NULL PRIMARY KEY label="Komanda",
    tasku_santykis INT NOT NULL label="Taškų santykis"
)
;quit;
proc sql;
CREATE TABLE Treneriai(
    trenerio_id INT NOT NULL PRIMARY KEY label="Trenerio id" check (trenerio_id between 10000 and 99999),
    pavarde CHAR(15) NOT NULL label="Pavardė",
    amzius INT NOT NULL label="Amžius",
    komanda CHAR(15)  NOT NULL label="Komanda"
)
;quit;

 

proc sql;
CREATE TABLE Buvusios_komandos(
    trenerio_id INT NOT NULL PRIMARY KEY label="Trenerio id" check (trenerio_id between 10000 and 99999),
    komanda CHAR(15) NOT NULL label="Komanda",
    metai int NOT NULL label="Trenerio id" check (metai between 1891 and YEAR(today()))
)
;quit;
proc sql;
CREATE TABLE Komandos(
    komanda CHAR(15) NOT NULL PRIMARY KEY label="Komanda",
    salis CHAR(15) NOT NULL label="Šalis",
    biudzetas INT NOT NULL label="Biudžetas" check (biudzetas > 0)
)
;quit;
proc sql;
CREATE TABLE zaidejai(
    zaidejo_id INT NOT NULL PRIMARY KEY label="Žaidėjo Id" check (zaidejo_id between 10000 and 99999),
    pavarde CHAR(15) NOT NULL label="Pavardė",
    komanda CHAR(15) NOT NULL label="Komanda",
    pozicija CHAR(15) NOT NULL label="Pozicija",
    amzius INT NOT NULL label="Amžius",
    numeris INT NOT NULL label="Numeris"
)
;quit;

proc sql;
CREATE TABLE Istorija(
	komanda CHAR(15) NOT NULL PRIMARY KEY label="Komanda",
	vieta_1	INT label = "1 vieta" check (vieta_1 between 0 and 100),
	vieta_2 INT label = "2 vieta" check (vieta_2 between 0 and 100),
	vieta_3 INT label = "3 vieta" check (vieta_3 between 0 and 100)
)
;quit;

proc sql;
ALTER TABLE zaidejai
	ADD CONSTRAINT i_komanda FOREIGN KEY(komanda) REFERENCES Turnyras ON DELETE RESTRICT ON UPDATE RESTRICT
;quit;

proc sql;
ALTER TABLE Treneriai
	ADD CONSTRAINT i_treneriai FOREIGN KEY(komanda) REFERENCES Turnyras ON DELETE RESTRICT ON UPDATE RESTRICT
;quit;



proc sql;
INSERT INTO Turnyras
VALUES(1, "Komanda 1", 29)
VALUES(2, "Komanda 2", 50)
VALUES(3, "Komanda 3", -15);

INSERT INTO Komandos
VALUES("Komanda 1", "Lietuva", 150000)
VALUES("Komanda 3", "Latvia", 1500)
VALUES("Komanda 2", "Estonia", 999999);

INSERT INTO Treneriai
VALUES(15992, "pavarde 1", 19, "Komanda 1")
VALUES(19922, "pavarde 3", 25, "Komanda 2")
VALUES(11555, "pavarde 2", 27, "Komanda 3");

INSERT INTO zaidejai
VALUES(29599, "zaidejas 1", "Komanda 1", "pozicija 1", 21, 23)
VALUES(29952, "zaidejas 5", "Komanda 2", "pozicija 3", 19, 8)
VALUES(37571, "zaidejas 3", "Komanda 3", "pozicija 2", 26, 6);

INSERT INTO Istorija
VALUES("Komanda 1", 19, 2, 15)
VALUES("Komanda 2", 2, 1, 15)
VALUES("Komanda 3", 3, 2, 26);

INSERT INTO Buvusios_komandos
VALUES(15992, "Komanda 6", 2000)
VALUES(19922, "Komanda 2", 1999)
VALUES(11555, "Komanda 25", 2010);
;quit;



