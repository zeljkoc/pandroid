package cilico.tools;

public class I2CTools {
	/**
	 * 读取RFID芯片的版本
	 * */
	static public native int readVersion();

	/**
	 * 读取IC卡的UID
	 * */
	static public native String ReadUID();

	/**
	 * 读取一块内容
	 * 
	 * @param pout
	 *            存放读到的数据
	 * @param PassWord
	 *            存放密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param add
	 *            块号，绝对路径
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int ReadBlock(byte[] pout, byte[] PassWord, byte type,
			byte add);

	/**
	 * 读取一个扇区内容
	 * 
	 * @param pout
	 *            存放读到的数据
	 * @param pnout
	 *            返回读到的数据长度,数组长度设为1
	 * @param PassWord
	 *            存放密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param add
	 *            扇区号
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int ReadSector(byte[] pout, byte[] pnout,
			byte[] PassWord, byte type, byte add);

	/**
	 * 写入一整个扇区
	 * 
	 * @param pin
	 *            存放写入的数据
	 * @param pnout
	 *            返回实际写入数据长度
	 * @param PassWord
	 *            存放密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param add
	 *            扇区号
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int WriteSector(byte[] pin, byte[] pnout,
			byte[] PassWord, byte type, byte add);

	/**
	 * 修改扇区密码
	 * 
	 * @param PassWord
	 *            存放现在密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param add
	 *            扇区号
	 * @param keyA
	 *            存放A密码（6位新密码）
	 * @param keyB
	 *            存放B密码（6位新密码）
	 * @param control
	 *            存放密码控制位（4位）
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int ChangeKeys(byte[] PassWord, byte type, int add,
			byte[] keyA, byte[] keyB, byte[] control);

	/**
	 * 读取多个连续的块，会自动跳过密码块
	 * 
	 * @param pout
	 *            存放读到的数据
	 * @param nout
	 *            返回读到的字节数
	 * @param PassWord
	 *            存放现在密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param addStat
	 *            起始块号（绝对块号）
	 * @param addEnd
	 *            结束块号（绝对块号）
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int ReadBlocksEx(byte[] pout, int[] nout,
			byte[] PassWord, byte type, int addStat, int addEnd);

	/**
	 * 从扇区的某一块开始读取一个扇区
	 * 
	 * @param pout
	 *            存放读到的数据
	 * @param pnout
	 *            返回读到的数据长度,数组长度设为1
	 * @param PassWord
	 *            存放密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param add
	 *            扇区号
	 * @param nStart
	 *            起始块号（相对块号）
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int ReadSectorEx(byte[] pout, byte[] nout,
			byte[] PassWord, byte type, byte add, byte nStart);

	/**
	 * 写一块内容
	 * 
	 * @param in
	 *            要写入的数据(该变量不能小于16字节)
	 * @param PassWord
	 *            存放密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param add
	 *            块号（绝对块号）
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int WriteBlock(byte[] in, byte[] PassWord, byte type,
			byte add);

	/**
	 * 从某一块开始写入一个扇区的内容，超过部分自动丢弃
	 * 
	 * @param in
	 *            存放写入的内容
	 * @param nout
	 *            返回实际写入的字节数
	 * @param PassWord
	 *            存放密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param add
	 *            扇区号
	 * @param nStart
	 *            起始块号（相对块号）
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int WriteSectorEx(byte[] in, byte[] nout,
			byte[] PassWord, byte type, byte add, byte nStart);

	/**
	 * 写入多个连续的块，会自动跳过密码块
	 * 
	 * @param in
	 *            存放写入的内容
	 * @param nout
	 *            返回实际写入的字节数
	 * @param PassWord
	 *            存放密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param addStat
	 *            起始块号（绝对块号）
	 * @param addEnd
	 *            结束块号（绝对块号）
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int WriteBlocksEx(byte[] in, int[] nout,
			byte[] PassWord, byte type, int addStat, int addEnd);

	/**
	 * 修改密码
	 * 
	 * @param PassWord
	 *            存放现在密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param addStar
	 *            起始扇区
	 * @param addEnd
	 *            结束扇区
	 * @param keyA
	 *            存放A密码（6位新密码）
	 * @param keyB
	 *            存放B密码（6位新密码）
	 * @param control
	 *            存放密码控制位（4位）
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int ChangeKeysEx(byte[] PassWord, byte type,
			byte addStar, byte addEnd, byte[] keyA, byte[] keyB, byte[] control);

	/**
	 * 初始化钱包
	 * 
	 * @param PassWord
	 *            存放现在密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param money
	 *            要写入的钱数
	 * @param add
	 *            要初始化为钱包的块号
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int InitWallet(byte[] PassWord, byte type, int money,
			byte add);

	/**
	 * 钱包加值
	 * 
	 * @param PassWord
	 *            存放密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param money
	 *            要加的钱数
	 * @param add
	 *            钱包的地址
	 * @return 成功返回0,否则返回错误码
	 */

	static public native int IncreaseWallet(byte[] PassWord, byte type,
			int money, byte add);

	/**
	 * 钱包减值
	 * 
	 * @param PassWord
	 *            存放密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param money
	 *            要减的钱数
	 * @param add
	 *            钱包的地址
	 * @return 成功返回0,否则返回错误码
	 */

	static public native int DecreaseWallet(byte[] PassWord, byte type,
			int money, byte add);

	/**
	 * 钱包的备份
	 * 
	 * @param PassWord
	 *            存放密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param add
	 *            钱包的地址（源地址）
	 * @param Desadd
	 *            备份的目标地址
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int BackupWallet(byte[] PassWord, byte type, byte add,
			byte Desadd);

	/**
	 * 读钱包
	 * 
	 * @param PassWord
	 *            存放密码
	 * @param type
	 *            密码类型 A密码为0x60,B密码为0x61
	 * @param money
	 *            存放读到的钱数
	 * @param add
	 *            钱包的地址（源地址）
	 * @return 成功返回0,否则返回错误码
	 */
	static public native int ReadWallet(byte[] PassWord, byte type,
			int[] money, byte add);

	static {
		System.loadLibrary("tiny-tools");
	}

	public static byte[] stringToBytes(String hexString) {
		if (hexString == null || hexString.equals("")) {
			return null;
		}
		hexString = hexString.toUpperCase();
		int length = hexString.length() / 2;
		char[] hexChars = hexString.toCharArray();
		byte[] d = new byte[length];
		for (int i = 0; i < length; i++) {
			int pos = i * 2;
			d[i] = (byte) (charToByte(hexChars[pos]) << 4 | charToByte(hexChars[pos + 1]));
		}
		return d;
	}

	private static byte charToByte(char c) {
		return (byte) "0123456789ABCDEF".indexOf(c);
	}

	public static String BytesToString(byte[] b) {
		String ret = "";
		for (int i = 0; i < b.length; i++) {
			String hex = Integer.toHexString(b[i] & 0xFF);
			if (hex.length() == 1) {
				hex = '0' + hex;
			}
			ret += hex.toUpperCase();
		}
		return ret;
	}

	public static int byteToInt(byte[] b) // byteToInt
	{
		int t2 = 0, temp = 0;
		for (int i = 3; i >= 0; i--) {
			t2 = t2 << 8;
			temp = b[i];
			if (temp < 0) {
				temp += 256;
			}
			t2 = t2 + temp;

		}
		return t2;

	}

	/**** int to byte ******/
	public static byte[] intToByte(int content, int offset) {
		// byte[] result = new byte[content.length << 2]; //乘以2的n次方 == 左移n位 即
		// content.length * 4 == content.length ＜＜ 2
		byte result[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
		for (int j = offset; j < result.length; j += 4) {
			result[j + 3] = (byte) (content & 0xff);
			result[j + 2] = (byte) ((content >> 8) & 0xff);
			result[j + 1] = (byte) ((content >> 16) & 0xff);
			result[j] = (byte) ((content >> 24) & 0xff);
		}
		return result;
	}

}
