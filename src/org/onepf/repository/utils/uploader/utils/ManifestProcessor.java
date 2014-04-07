package org.onepf.repository.utils.uploader.utils;

import org.onepf.repository.api.TextUtils;
import org.onepf.repository.api.responsewriter.descriptors.FeatureDescriptor;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.ByteArrayInputStream;
import java.util.*;

/**
 * Created by akarimova on 02.04.14.
 */
public class ManifestProcessor extends DefaultHandler {
    private static final Map<String, String[]> permissionsToFeatures;

    static {
        Map<String, String[]> tempMap = new HashMap<String, String[]>();
        //Bluetooth
        tempMap.put("android.permission.BLUETOOTH", new String[]{"android.hardware.bluetooth"});
        tempMap.put("android.permission.BLUETOOTH_ADMIN", new String[]{"android.hardware.bluetooth"});
        //Camera
        tempMap.put("android.permission.CAMERA", new String[]{"android.hardware.camera", "android.hardware.camera.autofocus"});
        //Location
        tempMap.put("android.permission.ACCESS_MOCK_LOCATION", new String[]{"android.hardware.location"});
        tempMap.put("android.permission.ACCESS_LOCATION_EXTRA_COMMANDS", new String[]{"android.hardware.location"});
        tempMap.put("android.permission.INSTALL_LOCATION_PROVIDER", new String[]{"android.hardware.location"});
        tempMap.put("android.permission.ACCESS_COARSE_LOCATION", new String[]{"android.hardware.location.network", "android.hardware.location"});
        tempMap.put("android.permission.ACCESS_FINE_LOCATION", new String[]{"android.hardware.location.gps", "android.hardware.location"});
        //Microphone
        tempMap.put("android.permission.RECORD_AUDIO", new String[]{"android.hardware.microphone"});
        //Telephony
        tempMap.put("android.permission.CALL_PHONE", new String[]{"android.hardware.telephony"});
        tempMap.put("android.permission.CALL_PRIVILEGED", new String[]{"android.hardware.telephony"});
        tempMap.put("android.permission.MODIFY_PHONE_STATE", new String[]{"android.hardware.telephony"});
        tempMap.put("android.permission.PROCESS_OUTGOING_CALLS", new String[]{"android.hardware.telephony"});
        tempMap.put("android.permission.READ_SMS", new String[]{"android.hardware.telephony"});
        tempMap.put("android.permission.RECEIVE_SMS", new String[]{"android.hardware.telephony"});
        tempMap.put("android.permission.RECEIVE_MMS", new String[]{"android.hardware.telephony"});
        tempMap.put("android.permission.RECEIVE_WAP_PUSH", new String[]{"android.hardware.telephony"});
        tempMap.put("android.permission.SEND_SMS", new String[]{"android.hardware.telephony"});
        tempMap.put("android.permission.WRITE_APN_SETTINGS", new String[]{"android.hardware.telephony"});
        tempMap.put("android.permission.WRITE_SMS", new String[]{"android.hardware.telephony"});
        //Wi-Fi
        tempMap.put("android.permission.ACCESS_WIFI_STATE", new String[]{"android.hardware.wifi"});
        tempMap.put("android.permission.CHANGE_WIFI_STATE", new String[]{"android.hardware.wifi"});
        tempMap.put("android.permission.CHANGE_WIFI_MULTICAST_STATE", new String[]{"android.hardware.wifi"});
        permissionsToFeatures = Collections.unmodifiableMap(tempMap);
    }

    //package
    private String packageName;
    //version
    private String versionName;
    private String versionCode;
    //sdk
    private int minSdkVersion;
    private int targetSdkVersion;
    private int maxSdkVersion;
    //permissions
    private Set<String> permissions = new HashSet<String>();
    //textures
    private Set<String> glTextures = new HashSet<String>();
    //features
    private Map<String, FeatureDescriptor> features = new HashMap<String, FeatureDescriptor>();
    //compatible screens
    private Set<Screen> cоmpatibleScreens = new HashSet<Screen>();


    public ManifestProcessor parse(String manifest) {
        SAXParserFactory saxParserFactory = SAXParserFactory.newInstance();
        try {
            SAXParser saxParser = saxParserFactory.newSAXParser();
            saxParser.parse(new ByteArrayInputStream(manifest.getBytes()), this);
        } catch (Exception e) {
            e.printStackTrace();
            //todo throw an exception
        }
        return this;
    }

    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes)
            throws SAXException {
        if (qName.equals("manifest")) {
            versionName = attributes.getValue("android:versionName");
            versionCode = attributes.getValue("android:versionCode");
            packageName = attributes.getValue("package");
        } else if (qName.equals("supports-gl-texture")) {
            glTextures.add(attributes.getValue("android:name"));
        } else if (qName.equals("supports-screens")) {
            SupportsScreen supportsScreen = new SupportsScreen();
            supportsScreen.smallScreens = booleanValue(attributes.getValue("android:smallScreens"), true);
            supportsScreen.resizeable = booleanValue(attributes.getValue("android:resizeable"), true);
            supportsScreen.normalScreens = booleanValue(attributes.getValue("android:normalScreens"), true);
            String largeScreens = attributes.getValue("android:largeScreens");
            supportsScreen.anyDensity = booleanValue(attributes.getValue("android:anyDensity"), true);
            //todo default values are not clear
            supportsScreen.requiresSmallestWidthDp = attributes.getValue("android:requiresSmallestWidthDp");
            supportsScreen.compatibleWidthLimitDp = attributes.getValue("android:compatibleWidthLimitDp");
            supportsScreen.largestWidthLimitDp = attributes.getValue("android:largestWidthLimitDp");
            //todo default values are different
            supportsScreen.largeScreens = TextUtils.isEmpty(largeScreens) ? null : Boolean.parseBoolean(largeScreens);
            String xLargeScreens = attributes.getValue("android:xlargeScreens");
            supportsScreen.xlargeScreens = TextUtils.isEmpty(xLargeScreens) ? null : Boolean.parseBoolean(xLargeScreens);
        } else if (qName.equals("uses-permission")) {
            permissions.add(attributes.getValue("android:name").trim());
        } else if (qName.equals("uses-sdk")) {
            minSdkVersion = integerValue(attributes.getValue("android:minSdkVersion"), 1);
            targetSdkVersion = integerValue(attributes.getValue("android:targetSdkVersion"), minSdkVersion);
            maxSdkVersion = integerValue(attributes.getValue("android:maxSdkVersion"), Integer.MAX_VALUE);
        } else if (qName.equals("uses-feature")) {
            FeatureDescriptor feature = new FeatureDescriptor();
            feature.setName(attributes.getValue("android:name"));
            feature.setRequired(booleanValue(attributes.getValue("android:required"), true));
            feature.setGlEsVersion(integerValue(attributes.getValue("android:glEsVersion"), 0x00010000)); //todo check default value
            features.put(feature.getName(), feature);
        } else if (qName.equals("screen")) {
            String value = attributes.getValue("android:screenSize");
            String value1 = attributes.getValue("android:screenDensity");
            Screen screen = new Screen();
            screen.setScreenSize(value);
            screen.setScreenDensity(value1);
            cоmpatibleScreens.add(screen);
        }
    }


    @Override
    public void endElement(String uri, String localName, String qName) throws SAXException {
        if (qName.equalsIgnoreCase("manifest")) {
            Set<String> impliedFeatures = new HashSet<String>();
            //get implied features
            for (String permission : permissions) {
                String[] featuresArray = permissionsToFeatures.get(permission);
                if (featuresArray != null && featuresArray.length > 0) {
                    Collections.addAll(impliedFeatures, featuresArray);
                }
            }
            for (String impliedFeature : impliedFeatures) {
                FeatureDescriptor feature = features.get(impliedFeature);
                if (feature == null) {
                    FeatureDescriptor newFeature = new FeatureDescriptor();
                    newFeature.setName(impliedFeature);
                    newFeature.setRequired(true);
                    features.put(newFeature.getName(), newFeature);
                }
            }
        }
    }


    @Override
    public void characters(char ch[], int start, int length) throws SAXException {
    }

    public boolean hasPermission(String permission) {
        return permissions.contains(permission);
    }

    public String getVersionName() {
        return versionName;
    }

    public String getVersionCode() {
        return versionCode;
    }

    public String getPackageName() {
        return packageName;
    }

    public static int integerValue(String s, int defaultValue) {
        if (TextUtils.isEmpty(s)) {
            return defaultValue;
        } else {
            return Integer.valueOf(s.trim());
        }
    }

    public static boolean booleanValue(String s, boolean defaultValue) {
        if (TextUtils.isEmpty(s)) {
            return defaultValue;
        } else {
            return Boolean.valueOf(s.trim());
        }
    }
}
