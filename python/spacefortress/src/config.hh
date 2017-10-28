#include "space-fortress.hh"

typedef enum {
  ConfigType_Double,
  ConfigType_Int,
  ConfigType_String,
} ConfigType;

typedef struct {
  ConfigType mType;
  // union {
    double mDouble;
    int mInt;
    std::string mString;
  // };
} ConfigValue;

class Config {
public:
  std::map<std::string, ConfigValue> mValues;
  Config();
  ~Config();

  void set(std::string key, std::string val);
  void set(std::string key, int val);
  void set(std::string key, double val);

  std::string getString(std::string key);
  double getDouble(std::string key);
  int getInt(std::string key);
};
