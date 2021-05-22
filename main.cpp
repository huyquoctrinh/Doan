#include <bits/stdc++.h>

using namespace std;

typedef int ll;
typedef long double ld;
typedef unsigned long long ull;
typedef pair <ll, ll> ii;
typedef pair <ll, ii> iii;

const ll N = 1e6 + 5;
const ll INF = 1e9 + 7;
const int base = 1000000000;
const int base_digits = 9;

clock_t start, End;
double cpu_time_used;

struct bigint {
    vector<int> a;
    int sign;

    bigint() :
        sign(1) {
    }

    bigint(long long v) {
        *this = v;
    }

    bigint(const string &s) {
        string ss = s;
        if (s[0] == '-') {
            sign = -1;
            ss.erase(0, 1);
        }
        read(ss);
    }

    void operator=(const bigint &v) {
        sign = v.sign;
        a = v.a;
    }

    void operator=(long long v) {
        sign = 1;
        if (v < 0)
            sign = -1, v = -v;
        for (; v > 0; v = v / base)
            a.push_back(v % base);
    }

    bigint operator+(const bigint &v) const {
        if (sign == v.sign) {
            bigint res = v;

            for (int i = 0, carry = 0; i < (int) max(a.size(), v.a.size()) || carry; ++i) {
                if (i == (int) res.a.size())
                    res.a.push_back(0);
                res.a[i] += carry + (i < (int) a.size() ? a[i] : 0);
                carry = res.a[i] >= base;
                if (carry)
                    res.a[i] -= base;
            }
            return res;
        }
        return *this - (-v);
    }

    bigint operator-(const bigint &v) const {
        if (sign == v.sign) {
            if (abs() >= v.abs()) {
                bigint res = *this;
                for (int i = 0, carry = 0; i < (int) v.a.size() || carry; ++i) {
                    res.a[i] -= carry + (i < (int) v.a.size() ? v.a[i] : 0);
                    carry = res.a[i] < 0;
                    if (carry)
                        res.a[i] += base;
                }
                res.trim();
                return res;
            }
            return -(v - *this);
        }
        return *this + (-v);
    }

    void operator*=(int v) {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = 0, carry = 0; i < (int) a.size() || carry; ++i) {
            if (i == (int) a.size())
                a.push_back(0);
            long long cur = a[i] * (long long) v + carry;
            carry = (int) (cur / base);
            a[i] = (int) (cur % base);
        }
        trim();
    }

    bigint operator*(int v) const {
        bigint res = *this;
        res *= v;
        return res;
    }

    friend pair<bigint, bigint> divmod(const bigint &a1, const bigint &b1) {
        int norm = base / (b1.a.back() + 1);
        bigint a = a1.abs() * norm;
        bigint b = b1.abs() * norm;
        bigint q, r;
        q.a.resize(a.a.size());

        for (int i = a.a.size() - 1; i >= 0; i--) {
            r *= base;
            r += a.a[i];
            int s1 = r.a.size() <= b.a.size() ? 0 : r.a[b.a.size()];
            int s2 = r.a.size() <= b.a.size() - 1 ? 0 : r.a[b.a.size() - 1];
            int d = ((long long) base * s1 + s2) / b.a.back();
            r -= b * d;
            while (r < 0)
                r += b, --d;
            q.a[i] = d;
        }

        q.sign = a1.sign * b1.sign;
        r.sign = a1.sign;
        q.trim();
        r.trim();
        return make_pair(q, r / norm);
    }

    bigint operator/(const bigint &v) const {
        return divmod(*this, v).first;
    }

    bigint operator%(const bigint &v) const {
        return divmod(*this, v).second;
    }

    void operator/=(int v) {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = (int) a.size() - 1, rem = 0; i >= 0; --i) {
            long long cur = a[i] + rem * (long long) base;
            a[i] = (int) (cur / v);
            rem = (int) (cur % v);
        }
        trim();
    }

    bigint operator/(int v) const {
        bigint res = *this;
        res /= v;
        return res;
    }

    int operator%(int v) const {
        if (v < 0)
            v = -v;
        int m = 0;
        for (int i = a.size() - 1; i >= 0; --i)
            m = (a[i] + m * (long long) base) % v;
        return m * sign;
    }

    void operator+=(const bigint &v) {
        *this = *this + v;
    }
    void operator-=(const bigint &v) {
        *this = *this - v;
    }
    void operator*=(const bigint &v) {
        *this = *this * v;
    }
    void operator/=(const bigint &v) {
        *this = *this / v;
    }

    bool operator<(const bigint &v) const {
        if (sign != v.sign)
            return sign < v.sign;
        if (a.size() != v.a.size())
            return a.size() * sign < v.a.size() * v.sign;
        for (int i = a.size() - 1; i >= 0; i--)
            if (a[i] != v.a[i])
                return a[i] * sign < v.a[i] * sign;
        return false;
    }

    bool operator>(const bigint &v) const {
        return v < *this;
    }
    bool operator<=(const bigint &v) const {
        return !(v < *this);
    }
    bool operator>=(const bigint &v) const {
        return !(*this < v);
    }
    bool operator==(const bigint &v) const {
        return !(*this < v) && !(v < *this);
    }
    bool operator!=(const bigint &v) const {
        return *this < v || v < *this;
    }

    void trim() {
        while (!a.empty() && !a.back())
            a.pop_back();
        if (a.empty())
            sign = 1;
    }

    bool isZero() const {
        return a.empty() || (a.size() == 1 && !a[0]);
    }

    bigint operator-() const {
        bigint res = *this;
        res.sign = -sign;
        return res;
    }

    bigint abs() const {
        bigint res = *this;
        res.sign *= res.sign;
        return res;
    }

    long long longValue() const {
        long long res = 0;
        for (int i = a.size() - 1; i >= 0; i--)
            res = res * base + a[i];
        return res * sign;
    }

    friend bigint gcd(const bigint &a, const bigint &b) {
        return b.isZero() ? a : gcd(b, a % b);
    }
    friend bigint lcm(const bigint &a, const bigint &b) {
        return a / gcd(a, b) * b;
    }

    void read(const string &s) {
        sign = 1;
        a.clear();
        int pos = 0;
        while (pos < (int) s.size() && (s[pos] == '-' || s[pos] == '+')) {
            if (s[pos] == '-')
                sign = -sign;
            ++pos;
        }
        for (int i = s.size() - 1; i >= pos; i -= base_digits) {
            int x = 0;
            for (int j = max(pos, i - base_digits + 1); j <= i; j++)
                x = x * 10 + s[j] - '0';
            a.push_back(x);
        }
        trim();
    }

    friend istream& operator>>(istream &stream, bigint &v) {
        string s;
        stream >> s;
        v.read(s);
        return stream;
    }

    friend ostream& operator<<(ostream &stream, const bigint &v) {
        if (v.sign == -1)
            stream << '-';
        stream << (v.a.empty() ? 0 : v.a.back());
        for (int i = (int) v.a.size() - 2; i >= 0; --i)
            stream << setw(base_digits) << setfill('0') << v.a[i];
        return stream;
    }

    static vector<int> convert_base(const vector<int> &a, int old_digits, int new_digits) {
        vector<long long> p(max(old_digits, new_digits) + 1);
        p[0] = 1;
        for (int i = 1; i < (int) p.size(); i++)
            p[i] = p[i - 1] * 10;
        vector<int> res;
        long long cur = 0;
        int cur_digits = 0;
        for (int i = 0; i < (int) a.size(); i++) {
            cur += a[i] * p[cur_digits];
            cur_digits += old_digits;
            while (cur_digits >= new_digits) {
                res.push_back(int(cur % p[new_digits]));
                cur /= p[new_digits];
                cur_digits -= new_digits;
            }
        }
        res.push_back((int) cur);
        while (!res.empty() && !res.back())
            res.pop_back();
        return res;
    }

    typedef vector<long long> vll;

    static vll karatsubaMultiply(const vll &a, const vll &b) {
        int n = a.size();
        vll res(n + n);
        if (n <= 32) {
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    res[i + j] += a[i] * b[j];
            return res;
        }

        int k = n >> 1;
        vll a1(a.begin(), a.begin() + k);
        vll a2(a.begin() + k, a.end());
        vll b1(b.begin(), b.begin() + k);
        vll b2(b.begin() + k, b.end());

        vll a1b1 = karatsubaMultiply(a1, b1);
        vll a2b2 = karatsubaMultiply(a2, b2);

        for (int i = 0; i < k; i++)
            a2[i] += a1[i];
        for (int i = 0; i < k; i++)
            b2[i] += b1[i];

        vll r = karatsubaMultiply(a2, b2);
        for (int i = 0; i < (int) a1b1.size(); i++)
            r[i] -= a1b1[i];
        for (int i = 0; i < (int) a2b2.size(); i++)
            r[i] -= a2b2[i];

        for (int i = 0; i < (int) r.size(); i++)
            res[i + k] += r[i];
        for (int i = 0; i < (int) a1b1.size(); i++)
            res[i] += a1b1[i];
        for (int i = 0; i < (int) a2b2.size(); i++)
            res[i + n] += a2b2[i];
        return res;
    }

    bigint operator*(const bigint &v) const {
        vector<int> a6 = convert_base(this->a, base_digits, 6);
        vector<int> b6 = convert_base(v.a, base_digits, 6);
        vll a(a6.begin(), a6.end());
        vll b(b6.begin(), b6.end());
        while (a.size() < b.size())
            a.push_back(0);
        while (b.size() < a.size())
            b.push_back(0);
        while (a.size() & (a.size() - 1))
            a.push_back(0), b.push_back(0);
        vll c = karatsubaMultiply(a, b);
        bigint res;
        res.sign = sign * v.sign;
        for (int i = 0, carry = 0; i < (int) c.size(); i++) {
            long long cur = c[i] + carry;
            res.a.push_back((int) (cur % 1000000));
            carry = (int) (cur / 1000000);
        }
        res.a = convert_base(res.a, 6, base_digits);
        res.trim();
        return res;
    }
};

inline string addBinary(const string &a, const string &b){
   string result = "";
   int temp = 0;
   int size_a = a.size() - 1;
   int size_b = b.size() - 1;
   while (size_a >= 0 || size_b >= 0 || temp == 1){
      temp += ((size_a >= 0)? a[size_a] - '0': 0);
      temp += ((size_b >= 0)? b[size_b] - '0': 0);
      result = char(temp % 2 + '0') + result;
      temp /= 2;
      size_a--; size_b--;
   }
   return result;
}

string intToString(int t) {
    string ret = "";
    while (t > 0) {
        ret += t % 10 + '0';
        t /= 10;
    }
    return ret;
}

inline string rev(string s) {
    reverse(s.begin(), s.end());
    return s;
}

inline string notBinary(const string &a) {
    string ret = "";
    for (auto c : a)
        if (c == '0') ret.push_back('1');
    else ret.push_back('0');
    return ret;
}

string decimalToBinary(bigint a) {
    string ret = "";
    bigint aa = a;
    aa = aa.abs();
    while (aa > 0) {
        ret.push_back(aa % 2 + '0');
        aa = aa / 2;
    }
    reverse(ret.begin(), ret.end());
    if (a.sign == -1) return addBinary(ret, "1");
    return ret;
}

bigint binaryToDecimal(const string &s) {
    bigint num = bigint(s);
    bigint dec_value = bigint(0);
    bigint base = bigint(1);
    bigint temp = num;
    while (temp > bigint(0)) {
        int last_digit = temp % 10;
        temp = temp / 10;
        dec_value = dec_value + (bigint(last_digit) * base);
        base = base * 2;
    }
    return dec_value;
}

int stringToInt(string s) {
    int ret = 0;
    for (auto c : s) ret = ret * 10 + c - '0';
    return ret;
}

void addZeros(string& str, int n)
{
    for (int i = 0; i < n; i++) str = "0" + str;
}

inline string stringToBase32(const string &s) {
    bigint u = bigint(s);
    string b32 = "";
    do
    {
        int d = u % 32;
        if (d < 10)
            b32.insert(0, 1, '0' + d) ;
        else
            b32.insert(0, 1, 'a' + d - 10) ;
        u = u / 32;
    } while(bigint(0) < u);
    return b32;
}

inline string stringToBase58(const string &s) {
    bigint u = bigint(s);
    string b58 = "";
    do
    {
        int d = u % 58;
        if (d < 10)
            b58.insert(0, 1, '0' + d) ;
        else
            b58.insert(0, 1, 'a' + d - 10) ;
        u = u / 58;
    } while(bigint(0) < u);
    return b58;
}

inline string stringToBase64(const string &s) {
    bigint u = bigint(s);
    string b64 = "";
    do
    {
        int d = u % 64;
        if (d < 10)
            b64.insert(0, 1, '0' + d) ;
        else
            b64.insert(0, 1, 'a' + d - 10) ;
        u = u / 64;
    } while(bigint(0) < u);
    return b64;
}

ll mulmod(ll a, ll b, ll mod)
{
    ll x = 0,y = a % mod;
    while (b > 0)
    {
        if (b % 2 == 1)
        {
            x = (x + y) % mod;
        }
        y = (y * 2) % mod;
        b /= 2;
    }
    return x % mod;
}

ll modulo(ll base, ll exponent, ll mod)
{
    ll x = 1;
    ll y = base;
    while (exponent > 0)
    {
        if (exponent % 2 == 1)
            x = (x * y) % mod;
        y = (y * y) % mod;
        exponent = exponent / 2;
    }
    return x % mod;
}

bool Miller(ll p)
{
    if (p < 2) return false;
    if (p != 2 && p % 2==0) return false;
    ll s = p - 1;
    while (s % 2 == 0) s = s / 2;
    for (int i = 0; i < 5; i++)
    {
        ll a = rand() % (p - 1) + 1, temp = s;
        ll mod = modulo(a, temp, p);
        while (temp != p - 1 && mod != 1 && mod != p - 1)
        {
            mod = mulmod(mod, mod, p);
            temp *= 2;
        }
        if (mod != p - 1 && temp % 2 == 0)
        {
            return false;
        }
    }
    return true;
}

string checkPrime(string s) {
    ll num = 0;
    for (auto c : s) num = num * 10 + c - '0';
    return Miller(num) ? "True" : "False";
}

string s;

int main (int argc, char *argv[])
{
    //freopen("QUOCHUY.INP", "r", stdin);
    freopen(argv[1], "r", stdin);
    freopen(argv[2], "w", stdout);
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);
    cout.tie(NULL);
    start = clock();
    while (getline(cin, s))
    {
        bool dau1 = false, dau2 = false;
        string s1 = "", s2 = "", pheptoan = "";
        if (s[0] == '2')
        {
            for (ll i = 2; i < s.size() and s[i] != ' '; i++)
                if (('0' <= s[i] and s[i] <= '9') or s[i] == '-') s1.push_back(s[i]);
            for (ll i = s.size(); i >= 2 and s[i] != ' '; i--)
                if (('0' <= s[i] and s[i] <= '9') or s[i] == '-') s2.push_back(s[i]);
            reverse(s2.begin(), s2.end());
            for (ll i = 3; i < s.size(); i++) {
                if (not ('0' <= s[i] and s[i] <= '9') and s[i] != ' ' and s[i] != '-')
                    pheptoan.push_back(s[i]);
                if (s[i] == '-' and i - 1 >= 0 and s[i - 1] == ' '
                    and i + 1 < s.size() and s[i + 1] == ' ') pheptoan.push_back(s[i]);
            }
            if (pheptoan == "+")
                cout << decimalToBinary(binaryToDecimal(s1) + binaryToDecimal(s2)) << endl;
            else if (pheptoan == "-")
                cout << decimalToBinary(binaryToDecimal(s1) - binaryToDecimal(s2)) << endl;
            else if (pheptoan == "*")
                cout << decimalToBinary(binaryToDecimal(s1) * binaryToDecimal(s2)) << endl;
            else if (pheptoan == "/")
                cout << decimalToBinary(binaryToDecimal(s1) / stringToInt(s2)) << endl;
            else if (pheptoan == "%")
                cout << decimalToBinary(binaryToDecimal(s1) % stringToInt(s2)) << endl;
            else if (pheptoan == "=") cout << ((s1 == s2) ? "True" : "False") << endl;
            else if (pheptoan != "") {
                int s1Len = s1.length();
                int s2Len = s2.length();
                if (s1Len > s2Len) addZeros(s2, s1Len - s2Len);
                else if (s2Len > s1Len) addZeros(s1, s2Len - s1Len);
                int len = max(s1Len, s2Len);
                string res = "";
                if (pheptoan == "&")
                    for (int i = 0; i < len; i++)
                        res.push_back(((s1[i] - '0') & (s2[i] - '0')) + '0');
                if (pheptoan == "|")
                    for (int i = 0; i < len; i++)
                        res.push_back(((s1[i] - '0') | (s2[i] - '0')) + '0');
                if (pheptoan == "^")
                    for (int i = 0; i < len; i++)
                        res.push_back(((s1[i] - '0') ^ (s2[i] - '0')) + '0');
                if (pheptoan == "~")
                    for (int i = 0; i < len; i++)
                        res.push_back(((s1[i] - '0') & (s2[i] - '0')) + '0');
                cout << res << endl;
            } else if (pheptoan == ">>") {
                string bin = s1;
                for (int i = 0; i < stringToInt(s2); i++) bin.pop_back();
                cout << bin << endl;
            }  else if (pheptoan == "<<") {
                string bin = s1;
                for (int i = 0; i < stringToInt(s2); i++) bin.push_back('0');
                cout << bin << endl;
            } else cout << binaryToDecimal(s2) << endl;
        }
        else
        {
            for (ll i = 3; i < s.size() and s[i] != ' '; i++)
                if (('0' <= s[i] and s[i] <= '9') or s[i] == '-') s1.push_back(s[i]);
            for (ll i = s.size(); i >= 3 and s[i] != ' '; i--)
                if (('0' <= s[i] and s[i] <= '9') or s[i] == '-') s2.push_back(s[i]);
            reverse(s2.begin(), s2.end());
            for (ll i = 3; i < s.size(); i++) {
                if (not ('0' <= s[i] and s[i] <= '9') and s[i] != ' ' and s[i] != '-')
                    pheptoan.push_back(s[i]);
                if (s[i] == '-' and i - 1 >= 0 and s[i - 1] == ' '
                    and i + 1 < s.size() and s[i + 1] == ' ') pheptoan.push_back(s[i]);
            }
            if (s.find("is_prime") != string::npos) cout << checkPrime(s2) << endl;
            else if (s.find("abs") != string::npos) cout << bigint(s2).abs() << endl;
            else if (s.find("to_base32") != string::npos) cout << stringToBase32(s2) << endl;
            else if (s.find("to_base58") != string::npos) cout << stringToBase58(s2) << endl;
            else if (s.find("to_base64") != string::npos) cout << stringToBase64(s2) << endl;
            else if (pheptoan == "+") cout << bigint(s1) + bigint(s2) << endl;
            else if (pheptoan == "-") cout << bigint(s1) - bigint(s2) << endl;
            else if (pheptoan == "*") cout << bigint(s1) * bigint(s2) << endl;
            else if (pheptoan == "/") cout << bigint(s1) / bigint(s2) << endl;
            else if (pheptoan == "%") cout << bigint(s1) % bigint(s2) << endl;
            else if (pheptoan == "=") cout << ((bigint(s1) == bigint(s2)) ? "True" : "False") << endl;
            else if (pheptoan == ">>") {
                string bin = decimalToBinary(bigint(s1));
                for (int i = 0; i < stringToInt(s2); i++) bin.pop_back();
                cout << binaryToDecimal(bin) << endl;
            }  else if (pheptoan == "<<") {
                string bin = decimalToBinary(bigint(s1));
                for (int i = 0; i < stringToInt(s2); i++) bin.push_back('0');
                cout << binaryToDecimal(bin) << endl;
            } else if (s.find("~") != string::npos) {
                if (s.find("-") != string::npos)
                    s2 = notBinary(decimalToBinary(bigint(s2)));
                cout << binaryToDecimal(notBinary(addBinary(s2, "1"))) << endl;
            } else if (pheptoan == "%") cout << bigint(s1) % stringToInt(s2) << endl;
            else if (pheptoan != "") {
                string a1 = decimalToBinary(s1), a2 = decimalToBinary(s2);
                int a1Len = a1.length();
                int a2Len = a2.length();
                if (a1Len > a2Len) addZeros(a2, a1Len - a2Len);
                else if (a2Len > a1Len) addZeros(a1, a2Len - a1Len);
                int len = max(a1Len, a2Len);
                string res = "";
                if (pheptoan == "&")
                    for (int i = 0; i < len; i++)
                        res.push_back(((a1[i] - '0') & (a2[i] - '0')) + '0');
                if (pheptoan == "|")
                    for (int i = 0; i < len; i++)
                        res.push_back(((a1[i] - '0') | (a2[i] - '0')) + '0');
                if (pheptoan == "^")
                    for (int i = 0; i < len; i++)
                        res.push_back(((a1[i] - '0') ^ (a2[i] - '0')) + '0');
                if (pheptoan == "~")
                    for (int i = 0; i < len; i++)
                        res.push_back(((a1[i] - '0') & (a2[i] - '0')) + '0');
                cout << binaryToDecimal(res) << endl;
            } else cout << decimalToBinary(bigint(s2)) << endl;
        }
    }
    End = clock();
    cpu_time_used = ((double) (End - start)) / CLOCKS_PER_SEC;
    cout <<"Time to do: " <<cpu_time_used<<endl;
    return 0;
}
