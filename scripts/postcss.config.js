const fs = require('fs');
const path = require('path');

const saveStyleModule = ({ srcDir, moduleNameSuffix }) => {
  return (cssFileName, json) => {
    const cssDir = path.dirname(cssFileName);
    const cssName = path.basename(cssFileName, '.css');
    const baseModuleNames = path.relative(srcDir, cssDir).split(path.sep);
    if (baseModuleNames.length > 0 && baseModuleNames[0] === '..')
      return;
    const moduleName = cssName + moduleNameSuffix;
    const pursFilePath = path.resolve(cssDir, moduleName + '.purs');
    const entries = Object
      .keys(json)
      .sort()
      .map((key) => ({ key, value: json[key] }))
      .map(({ key, value }) => {
        return {
          key: key.replace(/-(.)/, (_, c) => c.toUpperCase()),
          value
        };
      });
    const purs = [
      'module ' + baseModuleNames.concat([moduleName]).join('.')
    ].concat(
      '  ( ' + entries.map((i) => i.key).join('\n  , ') + '\n  ) where'
    ).concat(
      entries.map(({ key, value }) => {
        return [
          '',
          key + ' :: String',
          key + ' = "' + value + '"'
        ].join('\n');
      })
    ).join('\n');
    fs.writeFileSync(pursFilePath, purs, { encoding: 'utf-8' });
  };
};

const mySaveStyleModule = saveStyleModule({
  srcDir: path.resolve('./src'),
  moduleNameSuffix: 'Style'
});

module.exports = {
  plugins: [
    require('postcss-modules')({
      getJSON: (cssFileName, json, _outputFileName) => {
        return mySaveStyleModule(cssFileName, json);
      }
    })
  ]
};
