using NUnit.Framework;

namespace TestWrapper
{
    [TestFixture]
    public class OvertoppingCalculationTest
    {
        [Test]
        public static void TestOvertopping()
        {
            const double zExpected1A = 11.725;
            const double zExpected1B = 701.48866;
            const double margin = 0.00001;

            const int npoints = 3;
            var xcoords = new double[npoints];
            var ycoords = new double[npoints];
            var roughness = new double[npoints];

            const double dikeHeight = 9.1;
            var modelFactors = new OvertoppingInput
                {
                    FactorDeterminationQnFn = 2.3,
                    FactorDeterminationQbFb = 4.3,
                    Mz2 = 1.0,
                    Fshallow = 0.92,
                    ComputedOvertopping = 1,
                    CriticalOvertopping = 1,
                    Relaxationfactor = 1.0,
                    ReductionFactorForeshore = 0.5
                };

            const double criticalOvertoppingRate = 0.001;

            for (int i = 0; i < npoints; i++)
            {
                xcoords[i] = 5*(i + 1);
                ycoords[i] = 3 + 2*(i + 1);
                roughness[i] = 1;
            }

            const double normal = 60.0; // degrees

            var load1 = new OvertoppingLoadStruct
            { WaterLevel = 5.5, Direction = 50, Height = 1, Period = 4.0 };

            var result = OvertoppingFortranAccess.GetDischarge(load1, normal, xcoords, ycoords, roughness, dikeHeight, modelFactors);

            // this is not a benchmark, it only checks that the results do not change within 7 significant digits
            Assert.AreEqual(1.519737, result.Z2, 0.000001);
            Assert.AreEqual(8.089025E-09d, result.Qo, 1.0E-15);

            var z = OvertoppingFortranAccess.GetZValue(criticalOvertoppingRate, modelFactors, result.Qo);

            Assert.AreEqual(zExpected1A, z, margin, "Z value from dllOvertopping.dll");

            // no waves test:
            var load2 = new OvertoppingLoadStruct
            { WaterLevel = 5.5, Direction = 50, Height = 0, Period = 4.0 };

            result = OvertoppingFortranAccess.GetDischarge(load2, normal, xcoords, ycoords, roughness, dikeHeight, modelFactors);
            z = OvertoppingFortranAccess.GetZValue(criticalOvertoppingRate, modelFactors, result.Qo);
            Assert.AreEqual(z, zExpected1B, margin, "Z value from dllOvertopping.dll; no waves test");
        }

        [Test]
        public static void TestOvertoppingValidation()
        {
            const double dikeHeight = 9.1;
            var modelFactors = new OvertoppingInput
            {
                FactorDeterminationQnFn = 2.3,
                FactorDeterminationQbFb = 4.3,
                Mz2 = 1.0,
                Fshallow = 0.92,
                ComputedOvertopping = 1,
                CriticalOvertopping = 1,
                Relaxationfactor = 1.0,
                ReductionFactorForeshore = 0.5
            };

            var xcoords = new double[]{ 0, 10, 20, 30, 40 };
            var ycoords = new double[] {-5, 0, 5, 4, 0};
            var roughness = new [] {0.5, 0.5, 0.5, 0.5};

            string[] msg;
            var result = OvertoppingFortranAccess.Validate(xcoords, ycoords, roughness, dikeHeight, modelFactors, out msg);

            Assert.IsFalse(result, "validation");
            Assert.AreEqual(msg[0], "FOUT:Verticale coordinaten mogen niet afnemen.   5.00 en    4.00 doen dat wel.", "validation message");
            Assert.AreEqual(msg[1], "FOUT:Verticale coordinaten mogen niet afnemen.   4.00 en    0.00 doen dat wel.", "validation message");
        }

        [Test]
        public static void TestOvertoppingValidationMultiple()
        {
            const double dikeHeight = 9.1;
            var modelFactors = new OvertoppingInput
            {
                FactorDeterminationQnFn = 2.3,
                FactorDeterminationQbFb = 4.3,
                Mz2 = 1.0,
                Fshallow = -0.92,
                ComputedOvertopping = 1,
                CriticalOvertopping = 1,
                Relaxationfactor = 1.0,
                ReductionFactorForeshore = 0.5
            };

            var xcoords = new double[] { 0, 10, 20, 30, 40 };
            var ycoords = new double[] { -5, 0, 5, 4, 0 };
            var roughness = new[] { 0.5, 0.5, 0.5, 0.5 };

            string[] msg;
            var result = OvertoppingFortranAccess.Validate(xcoords, ycoords, roughness, dikeHeight, modelFactors, out msg);

            Assert.IsFalse(result, "validation");
            Assert.AreEqual(msg[0], "FOUT:Verticale coordinaten mogen niet afnemen.   5.00 en    4.00 doen dat wel.", "validation message");
            Assert.AreEqual(msg[1], "FOUT:Verticale coordinaten mogen niet afnemen.   4.00 en    0.00 doen dat wel.", "validation message");
            Assert.AreEqual(msg[2], "FOUT:Model factor fS (ondiepe golven) kleiner dan  0.000", "validation message");
        }
    }
}