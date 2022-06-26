#include <iostream>
#include <fstream>
#include <vector>
#include<cmath>
#include<iomanip>
#include<algorithm>

using std::cout;
using std::endl;
using std::vector;
using std::ofstream;
using std::setw;


void aparinkimas(int, vector<int>, vector<int>&);
int galingumas(int, vector <int>);
int cparinkimas(vector<int>, vector<int>&, int, int);
void sekos(int, int, int, vector<int>&);
void konvertavimas(int m, vector<int>, vector<float>&);
void maksimumas(int m, int k, vector<float>);
void keliniai(int, int, vector<float>);
void adgeneravimas(int, int, vector<float>);
void integralas(vector<float>);
void markovas(vector<float>&);
bool arPirminis(int);

int main()
{
    int m1 = 656, m2 = 810;
    vector<int> aVariantai1; //avariantai - vektorius skirtas laikyti visas tinkamas a reiksmes kazkokiam moduliui (siuo atveju 656)
    vector<int> aVariantai2; //skirtas 810 moduliui
    vector<int> pirminiai;
    vector<int> cmasyvas;
    vector<int> seka1, seka2;
    vector<float> tolyg1, tolyg2;

    pirminiai.push_back(2);
    pirminiai.push_back(3);
    //sukuriame pirminiu skaiciu vektoriu (mechaniskai irasome maksimalia verte)
    for (int i = 5; i <= 810; i++) {
        if (arPirminis(i)) {
            pirminiai.push_back(i);
        }
    }

    //atrenkame visus a variantus kiekvienam m:
    aparinkimas(m1, pirminiai, aVariantai1);
    aparinkimas(m2, pirminiai, aVariantai2);

    //renkame geriausia a pagal galia:
    int a1 = galingumas(m1, aVariantai1);
    cout << endl;

    int a2 = galingumas(m2, aVariantai2);
    cout << endl;

    //renkame c pagal maziausios koreliacijos teorini testa
    int c1 = cparinkimas(pirminiai, cmasyvas, m1, a1);
    cout << "c1 = " << c1 << endl;

    int c2 = cparinkimas(pirminiai, cmasyvas, m2, a2);
    cout << "c2 = " << c2 << endl;

    //spausdiname sekas
    sekos(a1, c1, m1, seka1);
    sekos(a2, 637, m2, seka2);

    //konvertuojame sekas i tolygias
    konvertavimas(m1, seka1, tolyg1);
    konvertavimas(m2, seka2, tolyg2);

    //testai
    //maksimumo
    maksimumas(m1, 6, tolyg1);
    maksimumas(m2, 6, tolyg2);
    //keliniu
    keliniai(m1, 3, tolyg1);
    keliniai(m2, 3, tolyg2);

    //a.d. generavimas
    adgeneravimas(m2, 3, tolyg2);

    //integralu skaiciavimas
    integralas(tolyg2);

    //markovo grandine
    markovas(tolyg2);

    //programos gale atlaisviname atminti
    pirminiai.clear();
    aVariantai1.clear();
    aVariantai2.clear();
    seka1.clear();
    seka2.clear();
    tolyg1.clear();
    tolyg2.clear();
}

void aparinkimas(int m, vector<int> pirminiai, vector<int> &amasyvas) {
    //atrenkame tik tuos a, kur jei p|m => p|a-1
    //tam pirma reikia atrinkti pirminius skaicius, kurie dalina m

    //pirma uzpildome vektoriu visais a variantais (galimais ir negalimais)
    for (int i = 2; i < m; i++) {
        amasyvas.push_back(i);
    }

    vector<int> temp;

    for(int sk : pirminiai){
        //tikriname, ar m dalinasi is kazkokio pirminio skaiciaus sk
        if (m % sk == 0) {
            //jei taip, tai visi a-1, esantys amasyve (avariantai1, avariantai2) turi dalintis is to pirminio sk
            for (int aSk : amasyvas) {
                if ((aSk - 1) % sk == 0) {
                    //jei tenkina salyga, tinkama a keliame i laikina vektoriu, kuri uzpilde padarome naujuoju avariantai vektoriumi
                    temp.push_back(aSk);
                }
            }
            amasyvas = temp;
            temp.clear();
        }
    }

    //toliau tikrinsime 4|m => 4|(a-1) (kodas beveik analogiskas ankstesniam)
    if (m % 4 == 0) {
        for (int aSk : amasyvas) {
            if ((aSk - 1) % 4 == 0) {
                temp.push_back(aSk);
            }
        }
        amasyvas = temp;
        temp.clear();
    }
}

//galingumo ieskojimas
int galingumas(int m, vector <int> amasyvas) {
    int index = 0;
    int iMax = 0;
    int galia = 0;
    int gMax = 0;
    long long bkelimas = 0;
    cout << "Galimi a su galiomis:" << endl;
    //einame per galimu a masyva (nustatytas anksciau)
    for (int aSk : amasyvas) {
        int n = 0;
        while (galia == 0) {
            n++;
            //keliame b laipsniu n tol, kol gauname kad jis yra m1 ar m2 kartotinis
            bkelimas = pow(aSk-1, n);
            if (bkelimas % m == 0) {
                galia = n;
                cout << "a = " << aSk << " su galia = " << galia << endl;
            }
        }
        //randame is visu a geriausia a, vis pakeisdami, jei atsiranda stipresnis. Jei lygus, nieko nedarom
        if (galia > gMax) {
            gMax = galia;
            iMax = index;
        }
        index++;
        galia = 0;
    }
    cout << "Galingiausias a buvo " << amasyvas[iMax] << " su galia " << gMax << endl;
    return amasyvas[iMax];
}

int cparinkimas(vector<int> pirminiai, vector<int>& cmasyvas, int m, int a) {
    vector<int> temp;
    //visu galimu c reiksmiu masyvas
    for (int i = 1; i < m; i++) {
        cmasyvas.push_back(i);
    }
    //atrenkame tiktuos c, kur nera bendro daliklio su m:
    for (int p:pirminiai) {
        if (m % p == 0) {
            cout << p << endl;
            for (int cSk : cmasyvas) {
                if (cSk % p != 0) {
                    temp.push_back(cSk);
                }
            }
        }
        cmasyvas = temp;
        temp.clear();
    }

    for (int c : cmasyvas) {
        cout << c << endl;
    }

    //2 c variantai (kai pridedame lygtyje ir kai atimame)

    int c_1, c_2;
    c_1 = ((3 + sqrt(3)) / 6) * m;

    c_2 = ((3 - sqrt(3)) / 6) * m;
    cout << c_1 << " " << c_2 << endl;

    //tikriname, ar rasti c tenkina p|m

    //dabar ieskosime, kurio koreliacija yra mazesne:
    float kor_c1, kor_c2;
    kor_c1 = 1 / (a*1.0) * (1 - 6 * c_1 / (m*1.0) + 6 * (c_1 / (m*1.0)) * (c_1 / (m*1.0)));
    cout << "kor1 = " << kor_c1 << endl;
    kor_c2 = 1 / (a * 1.0) * (1 - 6 * c_2 / (m * 1.0) + 6 * (c_2 / (m * 1.0)) * (c_2 / (m * 1.0)));
    cout << "kor2 = " << kor_c2 << endl;

    cmasyvas.clear();

    //tikriname, kurio koreliacija mazesne ir ta graziname
    if (kor_c1 < kor_c2) {
        return c_1;
    }
    else return c_2;
}

void sekos(int a, int c, int m, vector<int>& seka) {
    cout << "Seka (" << a << "X + " << c << ") mod " << m << ":" << endl;
    int x = 0;
    int counter = 2;
    cout << setw(3) << x << " ";
    do{
        x = (a * x + c) % m;
        seka.push_back(x);
        cout << setw(3) <<x << " ";
        if (counter%20 == 0) cout << "("<<counter/20 <<")"<< endl;
        counter++;
    } while (x != 0);
    cout << endl;
}

void konvertavimas(int m, vector<int> seka, vector<float>& tolyg) {
    float temp;
    for (int sk : seka) {
        temp = sk * 1.0 / (m * 1.0);
        tolyg.push_back(temp);
    }
}

void maksimumas(int m, int k, vector<float> tolyg) {
    float gr;
    float maxi = 0, temp;
    vector<float> maksimumai;
    gr = ceil(m * 1.0 / (k * 1.0));
    int pradz, pab;
    for (int i = 0; i < gr; i++) {
        pradz = i*k;
        if (pradz + k - 1 < m) {
            pab = pradz + k - 1;
        }
        else pab = m - 1;
        for (int j = pradz; j <= pab; j++) {
            temp = tolyg[j];
            if (temp > maxi) maxi = temp;
        }
        maksimumai.push_back(maxi);
        maxi = 0;
    }


    //surusiuojame vektoriu didejimo tvarka
    sort(maksimumai.begin(), maksimumai.end());

    maxi = -100;
    //ieskome maksimumo
    for (int i = 1; i <= gr; i++) {
        temp = (i*1.0) / gr - maksimumai[i];
        if (temp > maxi) maxi = temp;
    }
    cout << "m = " << m << "D vertes:" << endl;
    cout << "maksi: " << maxi << "; ";

    float maximinus = -100;
    for (int i = 1; i <= gr; i++) {
        temp = maksimumai[i] - ((i * 1.0) - 1) / gr;
        if (temp > maximinus) maximinus = temp;
    }
    cout << "maksiminus: " << maximinus << endl;

    maksimumai.clear();
}

void keliniai(int m, int k, vector<float> tolyg) {
    float gr;
    gr = floor(m * 1.0 / (k * 1.0));
    float temp[3];
    int pradz, pab;
    int v1 = 0, v2 = 0, v3 = 0, v4 = 0, v5 = 0, v6 = 0;
    int pos = 0;
    for (int i = 0; i < gr; i++) {
        pradz = i * k;
        pab = pradz + k - 1;

        for (int j = pradz; j <= pab; j++) {
            temp[pos] = tolyg[j];
            pos++;
        }

        //cia tikrink jau tuos if 6 turbut
        if      (temp[0] < temp[1] && temp[1] < temp[2]) v1++;
        else if (temp[0] < temp[2] && temp[2] < temp[1]) v2++;
        else if (temp[1] < temp[0] && temp[0] < temp[2]) v3++;
        else if (temp[1] < temp[2] && temp[2] < temp[0]) v4++;
        else if (temp[2] < temp[0] && temp[0] < temp[1]) v5++;
        else if (temp[2] < temp[1] && temp[1] < temp[0]) v6++;

        pos = 0;
    }
    cout << v1 << " " << v2 << " " << v3 << " " << v4 << " " << v5 << " " << v6 << endl;
}

void adgeneravimas(int m, int v, vector<float> tolyg) {
    int mtrecdalis = floor((m * 1.0) / 3);
    float V, x, s;
    vector<float> norm1, norm2, eksp, chi, stud;
    s = 0.7759;
    //norm seka skirta Y1
    for (int i = 0; i < mtrecdalis; i++) {
        V = 2 * tolyg[i] - 1;
        x = V * sqrt((-2 * log(s)) / s);
        norm1.push_back(x);
    }

    float V1, V2;
    s = pow(tolyg[mtrecdalis] * 2 - 1, 2) + pow(tolyg[mtrecdalis + 1] * 2 - 1, 2);
    cout << "s=" << s << endl;
    //norm seka skirta Y2
    for (int i = mtrecdalis; i < mtrecdalis*2; i++) {
        V = 2 * tolyg[i] - 1;
        x = V * sqrt((-2 * log(s)) / s);
        norm2.push_back(x);
    }

    //eksp seka skirta Y2
    for (int i = mtrecdalis*2; i < m-1; i++) {//m-1, nes paskutinis skaicius 0 (zr. zemiau)
        x = -1 * log(tolyg[i]);
        eksp.push_back(x);
    }
    eksp.push_back(0);//nes paskutinis narys pas mane sekoje yra 0, tai vietoje jo keiciu i 1, tada ln(1) gaunasi 0

    //chi kvadrat seka Y2
    for (int i = 0; i < mtrecdalis; i++) {
        x = 2 * eksp[i] + norm2[i] * norm2[i];
        chi.push_back(x);
    }

    //stud t seka
    for (int i = 0; i < mtrecdalis; i++) {
        x = norm1[i] * sqrt(v / chi[i]);
        stud.push_back(x);
    }

    norm1.clear();
    norm2.clear();
    eksp.clear();
    chi.clear();
    stud.clear();
}

void integralas(vector<float> tolyg) {
    float integ = 0;
    //tolygiai tolydziai pasiskirstes integralas
    for (auto& ad : tolyg) {
        if (ad != 0) {
            integ = integ + pow(2.71828, 4 * ad);
        }
    }
    integ = (4.0 / 809.0) * integ;
    //cout << "Tolygaus [0;4] integralas I = " << integ << endl;

    //tiesiskai pasiskirstes integralas
    integ = 0;
    for (auto& ad : tolyg) {
        if (ad != 0) {
            integ = integ + (pow(2.71828, 4 * sqrt(ad))) / sqrt(ad);
        } 
    }
    integ = (2.0 / 809.0) * integ;
    //cout << "Tiesiskai pasiskirsciusio a.d. integralas I = " << integ << endl;
}

void markovas(vector<float>& tolyg) {
    vector<float> MG;
    //inicijuojanti funkcija:
    if (tolyg[0] < 0.1) MG.push_back(1);
    else if (tolyg[0] < 0.3) MG.push_back(2);
    else if (tolyg[0] < 0.6) MG.push_back(3);
    else MG.push_back(4);

    int busena;

    //atnaujinimo funkcija:
    for (int i = 1; i < tolyg.size(); i++) {
        busena = MG.back();
        if (busena == 1) {
            if (tolyg[i] < 0.7) MG.push_back(1);
            else if (tolyg[i] < 0.9) MG.push_back(2);
            else MG.push_back(3);
        }

        else if (busena == 2) {
            if (tolyg[i] < 0.3) MG.push_back(1);
            else if (tolyg[i] < 0.9) MG.push_back(2);
            else MG.push_back(3);
        }

        else if (busena == 3) {
            if (tolyg[i] < 0.3) MG.push_back(1);
            else if (tolyg[i] < 0.5) MG.push_back(2);
            else MG.push_back(3);
        }

        else {
            if (tolyg[i] < 0.1) MG.push_back(1);
            else if (tolyg[i] < 0.5) MG.push_back(2);
            else if (tolyg[i] < 0.8) MG.push_back(3);
            else MG.push_back(4);
        }
    }

    for (auto& bus : MG) {
        cout << bus << "; ";
    }
    MG.clear();
}

//pagalbine funkcija, nustatanti, ar paduotas skacius pirminis
bool arPirminis(int n)
{
    if (n % 2 == 0 || n % 3 == 0)
        return false;

    for (int i = 5; i * i <= n; i = i + 6)
        if (n % i == 0 || n % (i + 2) == 0)
            return false;

    return true;
}