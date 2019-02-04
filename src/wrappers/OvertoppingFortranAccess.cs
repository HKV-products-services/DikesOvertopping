// Copyright (C) Stichting Deltares 2019. All rights reserved.
//
// This file is part of the Dikes Overtopping Kernel.
//
// The Dikes Overtopping Kernel is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
//
// All names, logos, and references to "Deltares" are registered trademarks of
// Stichting Deltares and remain full property of Stichting Deltares at all times.
// All rights reserved.

﻿using System;
using System.Runtime.InteropServices;
using System.Text;

namespace TestWrapper
{
    public struct OvertoppingLoadStruct
    {
        public double WaterLevel;
        public double Height;
        public double Period;
        public double Direction;
    }

    public struct OvertoppingInput
    {
        public double FactorDeterminationQnFn;  // model factor for non-breaking waves
        public double FactorDeterminationQbFb;  // model factor for breaking waves
        public double Mz2;                      // model factor describing the uncertainty of 2% runup height
        public double Fshallow;                 // model factor for shallow waves
        public double ComputedOvertopping;      // model factor computed overtopping
        public double CriticalOvertopping;      // model factor critical overtopping
        public double Relaxationfactor;         // relaxation factor iteration procedure wave runup
        public double ReductionFactorForeshore; // reduction factor foreshore
    }

    public struct OvertoppingResult
    {
        public double Z2;                       // 2% wave run-up (m)
        public double Qo;                       // wave overtopping discharge (m3/m per s)
    }

    public static class OvertoppingFortranAccess
    {
        private const int ErrorMessageLength = 255;
        private const int MaxFileSizeLength = 256;

        internal struct OvertoppingGeometryStruct
        {
            public double Normal;                   // dike normal (degrees)
            public int NPoints;                     // number of coordinates cross section 
            public IntPtr XCoords;                  // pointer to vector with x-coordinates cross section (m)
            public IntPtr YCoords;                  // pointer to vector with y-coordinates cross section (m+NAP)
            public IntPtr Roughness;                // pointer to vector with roughness factors cross section
        }

        public static OvertoppingResult GetDischarge(OvertoppingLoadStruct wave, double normal,
                                             double[] xCoords, double[] zCoords, double[] roughness,
                                             double dikeHeight, OvertoppingInput input)
        {

            var geometry = new OvertoppingGeometryStruct
                {
                    Normal = normal,
                    NPoints = xCoords.Length,
                    XCoords = Marshal.AllocHGlobal(Marshal.SizeOf(xCoords[0])*xCoords.Length),
                    YCoords = Marshal.AllocHGlobal(Marshal.SizeOf(zCoords[0])*zCoords.Length),
                    Roughness = Marshal.AllocHGlobal(Marshal.SizeOf(roughness[0])*roughness.Length)
                };
            Marshal.Copy(xCoords, 0, geometry.XCoords, xCoords.Length);
            Marshal.Copy(zCoords, 0, geometry.YCoords, zCoords.Length);
            Marshal.Copy(roughness, 0, geometry.Roughness, roughness.Length);

            var success = false;
            var errorMessage = new StringBuilder(ErrorMessageLength);
            OvertoppingResult result;
            result.Qo = double.NaN;
            result.Z2 = 0.0;
            var verbosity = -1;
            var logFile = new StringBuilder(MaxFileSizeLength);
            calculateQo(ref wave, ref geometry, ref dikeHeight, ref input, ref result, ref success, errorMessage,
                ref verbosity, logFile, errorMessage.Capacity, logFile.Capacity);

            Marshal.FreeHGlobal(geometry.XCoords);
            Marshal.FreeHGlobal(geometry.YCoords);
            Marshal.FreeHGlobal(geometry.Roughness);

            if (!success) { throw new Exception(ConvertString(errorMessage)); }
            return result;
        }

        public static double GetZValue(double criticalOvertoppingRate, OvertoppingInput input, double qo)
        {
            var success = false;
            var errorMessage = new StringBuilder(ErrorMessageLength);
            var z = double.NaN;

            calcZValue(ref criticalOvertoppingRate, ref input, ref qo, ref z, ref success, errorMessage, errorMessage.Capacity);
            if (!success) { throw new Exception(ConvertString(errorMessage)); }
            return z;
        }

        public static bool Validate(double[] xCoords, double[] zCoords, double[] roughness,
                                             double dikeHeight, OvertoppingInput input, out string[] errorMsg)
        {
            const int maxValidationMessages = 32;
            const int size = ErrorMessageLength * maxValidationMessages;

            var geometry = new OvertoppingGeometryStruct
            {
                Normal = 0,
                NPoints = xCoords.Length,
                XCoords = Marshal.AllocHGlobal(Marshal.SizeOf(xCoords[0]) * xCoords.Length),
                YCoords = Marshal.AllocHGlobal(Marshal.SizeOf(zCoords[0]) * zCoords.Length),
                Roughness = Marshal.AllocHGlobal(Marshal.SizeOf(roughness[0]) * roughness.Length)
            };
            Marshal.Copy(xCoords, 0, geometry.XCoords, xCoords.Length);
            Marshal.Copy(zCoords, 0, geometry.YCoords, zCoords.Length);
            Marshal.Copy(roughness, 0, geometry.Roughness, roughness.Length);

            var success = false;

            var longErrorMessage = new StringBuilder(size);
            ValidateInputC(ref geometry, ref dikeHeight, ref input, ref success, longErrorMessage, size);

            Marshal.FreeHGlobal(geometry.XCoords);
            Marshal.FreeHGlobal(geometry.YCoords);
            Marshal.FreeHGlobal(geometry.Roughness);

            if (success)
            {
                errorMsg = new[] { "" };
                errorMsg[0] = string.Empty;
            }
            else
            {
                errorMsg = ConvertString(longErrorMessage).Split('\t');
            }

            return success;
        }

        private static string ConvertString(StringBuilder message)
        {
            for (int i = ErrorMessageLength-1; i > 0; i--)
            {
                if (message[i] == ' ') continue;

                var smallCopiedString = new StringBuilder {Length = i + 1};
                for (int j = 0; j <= i; j++)
                {
                    smallCopiedString[j] = message[j];
                }
                var str = smallCopiedString.ToString();
                return str;
            }

            if (message.Length > ErrorMessageLength)
            {
                message[ErrorMessageLength] = '\0';
            }
            return message.ToString();  // something went wrong; fall back option
        }

        [DllImport("dllDikesOvertopping.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void calcZValue(ref double criticalOvertoppingRate, ref OvertoppingInput input,
            ref double qo, ref double z, ref bool success, StringBuilder message, int stringLength);

        [DllImport("dllDikesOvertopping.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void calculateQo(ref OvertoppingLoadStruct load, ref OvertoppingGeometryStruct geometry,
            ref double dikeHeight, ref OvertoppingInput input, ref OvertoppingResult result, ref bool success,
            StringBuilder message, ref int verbosity, StringBuilder logFile, int stringLength1, int stringLength2);

        [DllImport("dllDikesOvertopping.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void ValidateInputC(ref OvertoppingGeometryStruct geometry,
            ref double dikeHeight, ref OvertoppingInput input, ref bool succes, StringBuilder message, int stringLength);
    }
}
