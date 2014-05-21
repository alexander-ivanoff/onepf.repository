package com.company;

import org.apache.http.HttpEntity;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import sun.misc.BASE64Decoder;
import sun.misc.BASE64Encoder;

import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.security.*;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

public class SignRequester {

    private static final String SIGNATURE_ALGORITHM = "SHA1withRSA";
    private static final String KEY_ALGORITHM = "RSA";

    private static final String PUBLIC_KEY = "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDL920MD+/rtE3/UmVoxh6XpkJC\n" +
            "/PwXZO+Ehq1BC8mF4amjoJntQpr/Xt0yd/qI9Xr3LOTZ/ZSLAapAPQRqYzo9Adt7\n" +
            "u8asW5XdFLYMABStIo+R/koHgaATzrWmvEgqBJEgRYZcHkDdE/gfTLcxshTuytch\n" +
            "9ZoG2moW989gnBMqhQIDAQAB\n";

    private static final String APPSTORE_ID = "localstore";

    private static BASE64Decoder decoder = new BASE64Decoder();
    private static BASE64Encoder encoder = new BASE64Encoder();





    public static void main(String[] args) {
        if (args.length < 1)  {
            System.out.println("File path not given");
            System.exit(1);
        }

        String receiptData = args[0];
        String signature = getSignature(receiptData);
        String body = createBody(receiptData, signature, APPSTORE_ID);
        try {
            sendRequest(body, "http://localhost:8181/openaep/signReceipt");
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    private static String getSignature(String receiptData) {
        String signature = null;
        KeyPair keyPair = null;
        try {
            keyPair = loadKeyPair(KEY_ALGORITHM);
        } catch (Exception e) {

        }
        try {
            if (keyPair == null) {
                KeyPairGenerator keyGen = KeyPairGenerator.getInstance(KEY_ALGORITHM);
                SecureRandom random = SecureRandom.getInstance("SHA1PRNG");
                keyGen.initialize(1024, random);
                keyPair = keyGen.generateKeyPair();
                saveKeyPair(keyPair);
            }
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        if (keyPair != null) {
            try {
                signature = makeSignature(receiptData, keyPair.getPrivate());
                //boolean verified = verifySignature(receiptData, signature, keyPair.getPublic());
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return signature;
    }

    public static String makeSignature(String receiptData, PrivateKey key) throws Exception{
        Signature sig = Signature.getInstance(SIGNATURE_ALGORITHM);
        sig.initSign(key);
        sig.update(receiptData.getBytes());
        byte[] signatureBytes = sig.sign();
        return encoder.encode(signatureBytes);
    }





    public static String createBody(String receiptData, String signature, String appstoreId){
        String body = "<receipt version='1' receipt-data='%s' distributor-appstore='%s' distributor-signature='%s' />";
        return String.format(body, receiptData, appstoreId, signature);
    }


    public static void sendRequest(String request, String url) throws Exception{

        CloseableHttpClient httpclient = HttpClients.createDefault();
        try {
            HttpPost httppost = new HttpPost(url);
            httppost.setHeader("authToken", "W2K423WUYFm16SvY3d0Qh6iRu6uEZ7NQ");
            httppost.setEntity(new StringEntity(request));

                System.out.println("executing request " + httppost.getRequestLine());
                long uploadtime = System.currentTimeMillis();
                CloseableHttpResponse response = httpclient.execute(httppost);
                uploadtime = System.currentTimeMillis() - uploadtime;
                System.out.println("Upload time: " + uploadtime);
                try {
                    System.out.println("----------------------------------------");
                    System.out.println(response.getStatusLine());
                    int resultCode = response.getStatusLine().getStatusCode();

                    if (resultCode == HttpStatus.SC_OK) {
                        HttpEntity resEntity = response.getEntity();
                        EntityUtils.consume(resEntity);
                    } else {
                        throw new Exception(response.getStatusLine().toString());
                    }
                } catch (Exception e) {
                    System.out.println();

                } finally {
                    response.close();

                }
        } finally {
            httpclient.close();
        }
    }




    public static  void saveKeyPair(KeyPair keyPair) throws IOException {
        PrivateKey privateKey = keyPair.getPrivate();
        PublicKey publicKey = keyPair.getPublic();

// Store Public Key.
        X509EncodedKeySpec x509EncodedKeySpec = new X509EncodedKeySpec(
                publicKey.getEncoded());
        FileWriter writer = new FileWriter("public.key");
        writer.write(encoder.encode(x509EncodedKeySpec.getEncoded()));
        writer.close();

// Store Private Key.
        PKCS8EncodedKeySpec pkcs8EncodedKeySpec = new PKCS8EncodedKeySpec(
                privateKey.getEncoded());
        writer = new FileWriter("private.key");
        writer.write(encoder.encode(pkcs8EncodedKeySpec.getEncoded()));
        writer.close();
    }

    public static KeyPair loadKeyPair(String algorithm)
            throws IOException, NoSuchAlgorithmException,
            InvalidKeySpecException {
// Read Public Key.
        FileInputStream fis = new FileInputStream("public.key");

// Generate KeyPair.
        KeyFactory keyFactory = KeyFactory.getInstance(algorithm);
        X509EncodedKeySpec publicKeySpec = new X509EncodedKeySpec(
                decoder.decodeBuffer(fis));
        PublicKey publicKey = keyFactory.generatePublic(publicKeySpec);
        fis.close();

        // Read Private Key.
        fis = new FileInputStream("private.key");

        PKCS8EncodedKeySpec privateKeySpec = new PKCS8EncodedKeySpec(
                decoder.decodeBuffer(fis));
        PrivateKey privateKey = keyFactory.generatePrivate(privateKeySpec);
        fis.close();

        return new KeyPair(publicKey, privateKey);
    }

}