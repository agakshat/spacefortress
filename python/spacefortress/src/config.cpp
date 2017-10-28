#include "space-fortress.hh"

Config::Config() {
}

Config::~Config() {
}

void Config::set(std::string key, std::string val) {
  ConfigValue v;
  v.mString = val;
  v.mType = ConfigType_String;
  mValues[key] = v;
}

void Config::set(std::string key, double val) {
  ConfigValue v;
  v.mDouble = val;
  v.mType = ConfigType_Double;
  mValues[key] = v;
}

void Config::set(std::string key, int val) {
  ConfigValue v;
  v.mInt = val;
  v.mType = ConfigType_Int;
  mValues[key] = v;
}

std::string Config::getString(std::string key) {
  if( mValues.find(key) != mValues.end() ) {
    if( mValues[key].mType != ConfigType_String )
      printf( "Config mismatch: %s %d != %d\n", key.c_str(), mValues[key].mType, ConfigType_String );
    return mValues[key].mString;
  } else {
    printf( "No config key %s\n", key.c_str() );
    exit( 0 );
  }
}

double Config::getDouble(std::string key) {
  if( mValues.find(key) != mValues.end() ) {
    if( mValues[key].mType != ConfigType_Double )
      printf( "Config mismatch: %s %d != %d\n", key.c_str(), mValues[key].mType, ConfigType_Double );
    return mValues[key].mDouble;
  } else {
    printf( "No config key %s\n", key.c_str() );
    exit( 0 );
  }
}

int Config::getInt(std::string key) {
  if( mValues.find(key) != mValues.end() ) {
    if( mValues[key].mType != ConfigType_Int )
      printf( "Config mismatch: %s %d != %d\n", key.c_str(), mValues[key].mType, ConfigType_Int );
    return mValues[key].mInt;
  } else {
    printf( "No config key %s\n", key.c_str() );
    exit( 0 );
  }
}
