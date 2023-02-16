
# The headless CLI install given in this file is
# licensed under the MIT license by Timothy Sutton 2013 - 2014
# The unmodified code can be found at:
# https://github.com/timsutton/osx-vm-templates/blob/ce8df8a7468faa7c5312444ece1b977c1b2f77a4/scripts/xcode-cli-tools.sh#L8-L14
PROD=$(sudo softwareupdate -l |
           grep "\*.*Command Line" |
           tail -n 1 |
           awk -F"*" '{print $2}' |
           sed -e 's/^ *//' |
           sed 's/Label: //g' |
           tr -d '\n')
# s/Label: //g was required for Catalina

echo "[setup] Installing Xcode CLI via softwareupdate..."
# Install Xcode CLI
sudo softwareupdate -i "$PROD" --verbose;
