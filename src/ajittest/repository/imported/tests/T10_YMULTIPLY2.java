package ajittest.repository.imported.tests;

import java.util.Collection;
import org.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.skyscreamer.jsonassert.JSONAssert;
import iunit.utilities.testcase.execution.TestCaseExecutionHelper;
import iunit.utilities.xml.XMLUtils;
import com.arcadsoftware.iunit.rdi.core.utils.IUnitJSONComparator;
import java.util.List;
import java.util.Arrays;

@RunWith(value = Parameterized.class)
public class T10_YMULTIPLY2 {

	static String inputXMLFilePath = "testcases/ajittest.repository.imported.tests/T10_YMULTIPLY2.xml";
	String input;
	String expectedResult;
	String compareJson;
	String[] operator = new String[] { "*EQ", "*GE", "*GT", "*LE", "*LT", "*NE" };
	boolean isFailureAssertion = false;
	boolean isErrorAssertion = false;
	boolean isPass = false;

	public T10_YMULTIPLY2(String input, String expectedResult, String compareJson) {
		this.input = input;
		this.expectedResult = expectedResult;
		this.compareJson = compareJson;
	}

	@Parameters
	public static Collection<String[]> testData() {
		return XMLUtils.getTestData("resources/ajittest.repository.imported.tests/T10_YMULTIPLY2");
	}

	@BeforeClass
	public static void setUpIUnitTables() {
		TestCaseExecutionHelper.setUpTestExecutionEnv(inputXMLFilePath);
	}

	@Before
	public void loadPrerequisites() {
		TestCaseExecutionHelper.executeBeforePrerequisiteSteps(inputXMLFilePath);
	}

	@Test
	public void test() throws Exception {
		String actualResult = TestCaseExecutionHelper.executeTestCase(inputXMLFilePath, input, expectedResult,
				compareJson);
		if (!actualResult.trim().equalsIgnoreCase("") && !actualResult.substring(0, 5).equalsIgnoreCase("Error")) {
			if (isErrorAssertion) {
				isPass = false;
			} else {
				List<String> opList = Arrays.asList(operator);
				for (int i = 0; i < opList.size(); i++) {
					if (compareJson.contains(opList.get(i))) {
						isPass = IUnitJSONComparator.compareJson(expectedResult, actualResult, compareJson);
						break;
					}
				}
				if (!isPass && isFailureAssertion) {
					isPass = true;
				} else if (isPass && isFailureAssertion) {
					isPass = false;
				}
			}
		} else {
			if (isErrorAssertion) {
				isPass = true;
			} else {
				isPass = false;
			}
		}
		if (!isPass) {
			System.err.print("Test Case Status : Failure ");
			System.out.println("(for more details, right click on test node and choose iUnit -> show result).");
			throw new AssertionError();
		}

	}

	@After
	public void deletePrerequisites() {
		TestCaseExecutionHelper.executeAfterPrerequisiteSteps();
	}

	@AfterClass
	public static void terminateTestExecution() {
		TestCaseExecutionHelper.dropTestExecutionEnv(inputXMLFilePath);
	}
}