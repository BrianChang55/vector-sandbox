/**
 * Authentication hooks
 */
import { useMutation } from '@tanstack/react-query'
import { useNavigate } from 'react-router-dom'
import { useAppDispatch } from '../store/hooks'
import { logout as logoutAction } from '../store/slices/authSlice'

/**
 * Hook to handle user logout
 */
export function useLogout() {
  const navigate = useNavigate()
  const dispatch = useAppDispatch()
  
  return useMutation({
    mutationFn: async () => {
      // Clear auth state from Redux (this also clears localStorage via authService.logout())
      dispatch(logoutAction())
      
      return true
    },
    onSuccess: () => {
      // Redirect to login
      navigate('/login')
    },
  })
}

